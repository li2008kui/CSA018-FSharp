namespace ThisCoder.CSA018
open System
open System.Security.Cryptography
open System.Text
open MessageId
open Parameter
open ErrorCode
open StringHelper
open DESHelper

module MessageBody =
    type MessageBody(messageId: MessageId, gatewayId: uint32, luminaireId: uint32, parameterList: Parameter list, ?desKey: byte[]) =
        let mutable _messageId = messageId
        let mutable _gatewayId = gatewayId
        let mutable _luminaireId = luminaireId
        let mutable _parameterList = parameterList
        let mutable _errorCode = ErrorCode.Unknown
        let mutable _errorInfo = ""
        let _desKey: byte[] =
            if desKey.IsSome then desKey.Value
            else [||]
        new (messageId: MessageId, gatewayId: uint32, luminaireId: uint32, errorCode: ErrorCode, ?errorInfo: string, ?desKey: byte[]) as mb =
            mb.ErrorCode <- errorCode
            if errorInfo.IsSome then  mb.ErrorInfo <- errorInfo.Value
            if desKey.IsSome then
                MessageBody(messageId, gatewayId, luminaireId, [], desKey.Value)
            else
                MessageBody(messageId, gatewayId, luminaireId, [], null)
        member this.MessageId
            with get() = _messageId
            and set mi = _messageId <- mi
        member this.GatewayId
            with get() = _gatewayId
            and set gi = _gatewayId <- gi
        member this.LuminaireId
            with get() = _luminaireId
            and set li = _luminaireId <- li
        member this.ParameterList
            with get() = _parameterList
            and set pl = _parameterList <- pl
        member this.ErrorCode
            with get() = _errorCode
            and set ec = _errorCode <- ec
        member this.ErrorInfo
            with get() = _errorInfo
            and set ei = _errorInfo <- ei
        member this.DESKey
            with get() = _desKey
        member this.GetBody() =
            let mutable mb = [byte (this.MessageId >>> 8); byte this.MessageId]

            for i in [24..8..0] do
                mb <- mb @ [byte (this.GatewayId >>> i)]
            
            for i in [24..8..0] do
                mb <- mb @ [byte (this.LuminaireId >>> i)]

            if this.ParameterList.Length > 0 then
                for pmt in this.ParameterList do
                    mb <- mb @ Array.toList (pmt.GetParameter())
            else
                for i in [24..8..0] do
                    mb <- mb @ [byte (this.ErrorCode) >>> i]

                if this.ErrorInfo <> null then
                    mb <- mb @ (Array.toList (this.ErrorInfo.ToByteArray()))

            if this.DESKey <> null then
                if this.DESKey.Length <> 8 then
                    raise (ArgumentNullException("DESKey", "DES 密钥长度不正确。"))
                else
                    let des = new DESHelper()
                    des.Encrypt(this.DESKey, List.toArray mb)
            else
                List.toArray mb

        /// <summary>
        /// 获取消息体十六进制字符串。
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
            let mb = this.GetBody()

            for i = 0 to mb.Length - 1 do
                if i < mb.Length - 1 then
                    hexString <- (mb.[i]).ToString("X2") + sp
                else
                    hexString <- (mb.[i]).ToString("X2")

        /// <summary>
        /// 获取消息体字符串。
        /// </summary>
        /// <returns></returns>
        override this.ToString() =
            Encoding.UTF8.GetString(this.GetBody())