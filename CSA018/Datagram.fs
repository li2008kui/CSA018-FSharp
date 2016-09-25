namespace ThisCoder.CSA018
open System
open System.Collections.Generic
open System.Text
open MessageHead
open MessageBody
open MessageType
open CsaException
open ErrorCode
open DESHelper
open MessageId
open BytesHelper
open Parameter
open Crc32

module Datagram =
    type Datagram(head: MessageHead, body: MessageBody) =
        let mutable _head = head
        let mutable _body = body
        let mutable _isCryptographic =
            try
                body.DESKey.Length = 8
            with
                | _ -> false
        new (head: MessageHead) as d = Datagram(head, d.Body)
        new () as d = Datagram(d.Head, d.Body)
        member this.Stx
            with get() = 0x02uy
        member this.Head
            with get() = _head
            and set h = _head <- h
        member this.Body
            with get() = _body
            and set b = _body <- b
        member this.Etx
            with get() = 0x03uy
        member this.IsCryptographic
            with get() = _isCryptographic
            and set c = _isCryptographic <- c

        /// <summary>
        /// 获取消息报文字节数组。
        /// </summary>
        /// <returns>消息报文字节数组。</returns>
        member this.GetDatagram() =
            match this.Head.Type with
            | MessageType.HeartbeatData -> [|0xFFuy|]
            | MessageType.HeartbeatResponse -> [|0xFEuy|]
            | _ ->
                let mutable d = []
                let h = Datagram.Descaping(this.Head.GetHead())
                d <- Array.toList h

                if this.Head.Type <> MessageType.CommandACK
                    && this.Head.Type <> MessageType.EventACK then
                        let b = Datagram.Descaping(this.Body.GetBody())
                        d <- Array.toList b

                d <- [this.Etx]
                List.toArray d

        /// <summary>
        /// 获取消息报文对象列表。
        /// </summary>
        /// <param name="dataArray">消息报文字节数组。</param>
        /// <param name="desKey">
        /// DES 密钥，默认不加密。
        /// <para>该密钥运算模式采用 ECB 模式。</para>
        /// </param>
        /// <param name="isTcpOrUdp">报文承载方式是否是TCP或UDP，默认为false。</param>
        /// <param name="isCheckCrc">是否校验CRC。</param>
        /// <returns>消息报文对象列表。</returns>
        static member GetDatagramList(dataArray: byte [], ?desKey: byte [], ?isTcpOrUdp: bool, ?isCheckCrc: bool) =
            let datagramList = new List<Datagram>()
            let dataList = new List<byte>(dataArray)

            // 移除末尾的0
            for i = dataArray.Length downto 0 do
                if dataArray.[i] = 0uy then
                    dataList.RemoveAt(i)

            if dataList.Count < 15 then
                for b in dataList do
                    match b with
                    | 0xffuy ->
                        let h = new MessageHead(MessageType.HeartbeatData)
                        datagramList.Add(new Datagram(h))
                    | 0xfeuy ->
                        let h = new MessageHead(MessageType.HeartbeatResponse)
                        datagramList.Add(new Datagram(h))
                    | _ -> 1 |> ignore

                raise (new CsaException("消息解析错误。", ErrorCode.MessageParseError))

            if datagramList.Count = 0 then
                let mutable newByteArrayList = new List<byte []>()

                if isTcpOrUdp.IsNone || isTcpOrUdp.Value = false then
                    let mutable byteArrayList = new List<byte[]>()
                    byteArrayList <- Datagram.GetByteArrayList(dataList.ToArray(), 0, byteArrayList)
                    newByteArrayList <- Datagram.Descaping(byteArrayList)
                else
                    newByteArrayList.Add(dataList.ToArray())

                for tempByteArray in newByteArrayList do
                    if tempByteArray.Length > 15 then
                        if Enum.IsDefined(typeof<MessageType>, tempByteArray.[0]) = false then
                            raise (new CsaException("参数类型未定义。", ErrorCode.ParameterTypeUndefined))

                        let d = new Datagram()
                        let mh = new MessageHead()
                        mh.Type <- enum<MessageType> (int tempByteArray.[0])
                        mh.SeqNumber <- (uint32 (tempByteArray.[1] <<< 24) + uint32 (tempByteArray.[2] <<< 16) + uint32 (tempByteArray.[3] <<< 8) + uint32 tempByteArray.[4])
                        mh.Length <- (uint16 (tempByteArray.[5] <<< 8) + uint16 tempByteArray.[6])
                        //mh.Reserved <- (uint64 (tempByteArray.[7] <<< 32) + uint64 (tempByteArray.[8] <<< 24) + uint64 (tempByteArray.[9] <<< 16) + uint64 (tempByteArray.[10] <<< 8) + uint64 tempByteArray.[11])
                        mh.Crc32 <- (uint32 (tempByteArray.[12] <<< 24) + uint32 (tempByteArray.[13] <<< 16) + uint32 (tempByteArray.[14] <<< 8) + uint32 tempByteArray.[15])

                        if mh.Type = MessageType.Command
                        || mh.Type = MessageType.Event
                        || mh.Type = MessageType.CommandResult then
                            let newByteArray = Array.sub tempByteArray 16 (int mh.Length)
                            let mutable msgBody: byte [] = null

                            if desKey.IsSome && desKey.Value.Length = 8 then
                                let des = new DESHelper()
                                msgBody <- des.Decrypt(desKey.Value, newByteArray)

                                d.IsCryptographic <- true
                            else
                                msgBody <- newByteArray

                            if msgBody.Length >= 10 then
                                if Enum.IsDefined(typeof<MessageId>, (uint16 (msgBody.[0] <<< 8) + uint16 msgBody.[1])) = false then
                                    raise (new CsaException("消息ID未定义。", ErrorCode.MessageIdUndefined))

                                let mb = new MessageBody()
                                mb.MessageId <- enum<MessageId> (int (msgBody.[0] <<< 8) + int msgBody.[1])
                                mb.GatewayId <- (uint32 (msgBody.[2] <<< 24) + uint32 (msgBody.[3] <<< 16) + uint32 (msgBody.[4] <<< 8) + uint32 msgBody.[5])
                                mb.LuminaireId <- (uint32 (msgBody.[6] <<< 24) + uint32 (msgBody.[7] <<< 16) + uint32 (msgBody.[8] <<< 8) + uint32 msgBody.[9])

                                match mh.Type with
                                | MessageType.CommandResult ->
                                    mb.ErrorCode <- enum<ErrorCode> (int32 (msgBody.[10] <<< 24) + int32 (msgBody.[11] <<< 16) + int32 (msgBody.[12] <<< 8) + int32 msgBody.[13])
                                    let errorInfoArrayList = new List<byte>()

                                    for i = 14 to msgBody.Length do
                                        errorInfoArrayList.Add(msgBody.[i])

                                    if errorInfoArrayList.Count > 0 then
                                        mb.ErrorInfo <- ByteArray.ToString2(errorInfoArrayList.ToArray())
                                | _ ->
                                    mb.ParameterList <- Parameter.GetParameterList(msgBody, 10)

                                if isCheckCrc.IsSome && isCheckCrc.Value && Crc32.GetCrc32(newByteArray) <> mh.Crc32 then
                                    raise (new CsaException("消息体CRC校验错误。", ErrorCode.ChecksumError))

                                d.Body <- mb
                            else
                                raise (new CsaException("消息解析错误。", ErrorCode.MessageParseError))

                        d.Head <- mh
                        datagramList.Add(d)
            datagramList

        /// <summary>
        /// 转义特殊字符。
        /// <para>STX转义为ESC和0xE7，即02->1BE7。</para>
        /// <para>ETX转义为ESC和0xE8，即03->1BE8。</para>
        /// <para>ESC转义为ESC和0x00，即1B->1B00。</para>
        /// </summary>
        /// <param name="byteArray">消息报文字节数组。</param>
        /// <returns>转义后的字节数组。</returns>
        static member private Escaping(byteArray: byte []) =
            let byteList = new List<byte>()

            for item in byteArray do
                match item with
                | 0x02uy ->
                    byteList.Add(0x1buy)
                    byteList.Add(0xe7uy)
                | 0x03uy ->
                    byteList.Add(0x1buy)
                    byteList.Add(0xe8uy)
                | 0x1buy ->
                    byteList.Add(0x1buy)
                    byteList.Add(0x00uy)
                | _ ->
                    byteList.Add(item)

            byteList.ToArray()

        /// <summary>
        /// 去除转义特殊字符。
        /// </summary>
        /// <param name="byteArray">原消息报文字节数组。</param>
        /// <returns>去除转义字符的字节数组。</returns>
        static member private Descaping(byteArray: byte []) =
            let mutable bl = []
            let mutable i = 0

            while i < byteArray.Length do
                match byteArray.[i] with
                | 0x1buy ->
                    if i + 1 < byteArray.Length then
                        match byteArray.[i + 1] with
                        | 0xe7uy -> bl <- bl @ [0x02uy]
                        | 0xe8uy -> bl <- bl @ [0x03uy]
                        | 0x00uy -> bl <- bl @ [0x1buy]
                        | _ -> bl <- bl @ [byteArray.[i + 1]]

                    i <- i + 2
                | _ ->
                    bl <- bl @ [byteArray.[i]]
                    i <- i + 1

            List.toArray bl

        /// <summary>
        /// 去除转义特殊字符。
        /// </summary>
        /// <param name="byteArrayList">原消息报文字节数组列表。</param>
        /// <returns>去除转义字符的字节数组列表。</returns>
        static member private Descaping(byteArrayList: List<byte []>) =
            let newByteArrayList = new List<byte[]>()
            let mutable byteArray: byte [] = null

            for item in byteArrayList do
                byteArray <- Datagram.Descaping(item)
                newByteArrayList.Add(byteArray)

            newByteArrayList

        /// <summary>
        /// 获取消息报文字节数组列表。
        /// <para>此列表中的消息报文字节数组不包含起止符。</para>
        /// </summary>
        /// <param name="dataArray">消息报文字节数组。</param>
        /// <param name="index">数组索引。</param>
        /// <param name="byteArrayList">消息报文字节数组列表。</param>
        static member private GetByteArrayList(dataArray: byte [], index: int, byteArrayList: List<byte []>) =
            let rec getbal (da: byte []) idx ref (bal: List<byte []>) =
                let mutable isStx = false
                let mutable byteList = new List<byte>()

                for i = index to da.Length do
                    match da.[i] with
                    | 0x02uy ->
                        isStx <- true
                        byteList <- new List<byte>()
                    | 0x03uy ->
                        isStx <- false

                        if byteList.Count > 0 then
                            bal.Add(byteList.ToArray())

                        getbal dataArray (i + 1) ref bal
                        //break
                    | _ ->
                        if isStx then
                            byteList.Add(da.[i])
            
            //let mutable baList = byteArrayList
            getbal dataArray index ref byteArrayList
            byteArrayList

        /// <summary>
        /// 获取消息报文十六进制字符串。
        /// </summary>
        /// <param name="separator">
        /// 分隔符。
        /// <para>默认为空字符。</para>
        /// </param>
        /// <returns>消息报文十六进制字符串。</returns>
        member this.ToHexString(?separator: string) =
            let mutable sp = " "
            if separator.IsSome then sp <- separator.Value

            let mutable hexString = ""
            let d = this.GetDatagram()

            for i = 0 to d.Length - 1 do
                if i < d.Length - 1 then
                    hexString <- (d.[i]).ToString("X2") + sp
                else
                    hexString <- (d.[i]).ToString("X2")

        /// <summary>
        /// 获取消息报文字符串。
        /// </summary>
        /// <returns>消息报文字符串。</returns>
        override this.ToString() =
            Encoding.UTF8.GetString(this.GetDatagram())