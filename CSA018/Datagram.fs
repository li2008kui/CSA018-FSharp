namespace ThisCoder.CSA018
open MessageHead
open MessageBody
open MessageType

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
        /// 去除转义特殊字符。
        /// </summary>
        /// <param name="byteArray">原消息报文字节数组。</param>
        /// <returns>去除转义字符的字节数组。</returns>
        static member Descaping(byteArray: byte []) =
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