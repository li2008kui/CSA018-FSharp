namespace ThisCoder.CSA018
open System
open System.Text
open MessageType

module MessageHead =
    type MessageHead(type': MessageType, seqNumber: uint32, length: uint16, crc32: uint32) =
        let mutable _type = type'
        let mutable _seqNumber = seqNumber
        let mutable _length = length
        let mutable _crc32 = crc32
        new (type': MessageType, length: uint16, crc32: uint32) =
            MessageHead(type', 0ul, length, crc32)
        new (type': MessageType, seqNumber: uint32) =
            MessageHead(type', seqNumber, 0us, 0ul)
        new (type': MessageType) =
            MessageHead(type', 0ul, 0us, 0ul)
        member this.Type
            with get() = _type
            and set t = _type <- t
        member this.SeqNumber
            with get() = _seqNumber
            and set s = _seqNumber <- s
        member this.Length
            with get() = _length
            and set l = _length <- l
        member this.Reserved
            with get() = 0x0000000000
        member this.Crc32
            with get() = _crc32
            and set c = _crc32 <- c

        /// <summary>
        /// 获取消息头字节数组。
        /// </summary>
        /// <returns></returns>
        member this.GetHead() =
            let mutable mh =[byte this.Type]

            for i in [24..8..0] do
                mh <- mh @ [byte (this.SeqNumber >>> i)]

            mh <- mh @ [byte (this.Length >>> 8); byte this.Length]
            mh <- mh @ [0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy]

            for i in [24..8..0] do
                mh <- mh @ [byte (this.Crc32 >>> i)]

            List.toArray mh

        /// <summary>
        /// 获取消息头十六进制字符串。
        /// </summary>
        /// <param name="separator">
        /// 分隔符。
        /// <para>默认为空字符。</para>
        /// </param>
        /// <returns></returns>
        member this.ToHexString(?separator: string) =
            let mutable sp = " "
            if separator.IsSome then sp <- separator.Value

            let mutable hexString = ""
            let mh = this.GetHead()

            for i = 0 to mh.Length - 1 do
                if i < mh.Length - 1 then
                    hexString <- (mh.[i]).ToString("X2") + sp
                else
                    hexString <- (mh.[i]).ToString("X2")

        /// <summary>
        /// 获取消息头字符串。
        /// </summary>
        /// <returns></returns>
        override this.ToString() =
            Encoding.UTF8.GetString(this.GetHead())