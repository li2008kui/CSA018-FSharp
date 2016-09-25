namespace ThisCoder.CSA018
open System
open System.Text
open System.Collections.Generic
open ParameterType
open CsaException
open ErrorCode

module Parameter =
    type Parameter(type': ParameterType, value: byte[]) =
        let mutable _type = type'
        let mutable _value = value
        new (type': ParameterType, value: string) =
            Parameter(type', Encoding.UTF8.GetBytes(value))
        new (type': ParameterType, value: byte) =
            Parameter(type', [|value|])
        new () = Parameter(ParameterType.GatewayId, 0x00uy)
        member this.Type
            with get() = _type
            and set t = _type <- t
        member this.Value
            with get() = _value
            and set v = _value <- v
        member this.End
            with get() = 0x00uy
        member this.GetParameter() =
            let mutable pmt = [byte (this.Type >>> 8); byte this.Type]
            pmt <- pmt @ Array.toList this.Value
            pmt <- pmt @ [0x00uy]
            List.toArray pmt

        static member private CheckParameterValue(type': ParameterType, value: byte []) =
            match type' with
            | ParameterType.GatewayId ->
                if value.Length <> 4 then
                    raise (new CsaException("参数长度错误。", ErrorCode.ParameterLengthError))

                let gatewayId = (uint32 value.[0] <<< 24) + (uint32 value.[1] <<< 16) + (uint32 value.[2] <<< 8) + (uint32 value.[3])

                if gatewayId < 1ul || gatewayId > 0xfffffffful then
                    raise (new CsaException("参数范围错误。", ErrorCode.ParameterScopeError))
            | ParameterType.LuminaireId ->
                if value.Length <> 4 then
                    raise (new CsaException("参数长度错误。", ErrorCode.ParameterLengthError))

                let luminaireId = (uint32 value.[0] <<< 24) + (uint32 value.[1] <<< 16) + (uint32 value.[2] <<< 8) + (uint32 value.[3])

                if luminaireId < 1ul || luminaireId > 0xffffff00ul then
                    raise (new CsaException("参数范围错误。", ErrorCode.ParameterScopeError))
            | _ ->
                1 |> ignore

        /// <summary>
        /// 获取参数对象列表。
        /// </summary>
        /// <param name="byteArray">消息报文字节数组。</param>
        /// <param name="index">数组索引。</param>
        static member internal GetParameterList(byteArray: byte [], index: int) =
            let pmtList = new List<Parameter>();
            let mutable idx = index
            let rec getpl() =
                if byteArray.Length > idx + 2 then
                    let parameter = new Parameter()
                    let byteList = new List<byte>()
                    let pmtType = (uint16 byteArray.[idx] <<< 8) + (uint16 byteArray.[idx + 1])

                    if Enum.IsDefined(typeof<ParameterType>, pmtType) = false then
                        raise (new CsaException("参数类型未定义。", new Exception("若参数类型确实已经定义，请查看参数值是否包含参数结束符0x00。"), ErrorCode.ParameterTypeUndefined))

                    parameter.Type <- enum<ParameterType> (int pmtType)

                    match parameter.Type with
                    | ParameterType.GatewayId
                    | ParameterType.LuminaireId ->
                        byteList.Add(byteArray.[idx + 2])
                        byteList.Add(byteArray.[idx + 3])
                        byteList.Add(byteArray.[idx + 4])
                        byteList.Add(byteArray.[idx + 5])
                        idx <- idx + 4
                    | _ ->
                        let mutable indexByte = byteArray.[idx + 2]

                        while indexByte <> 0x00uy do
                            byteList.Add(indexByte)
                            idx <- idx + 1
                            indexByte <- byteArray.[idx + 2]
                            
                    parameter.Value <- byteList.ToArray()
                    pmtList.Add(parameter)

                    // 递归调用
                    idx <- idx + 3
                    getpl()
            // 调用递归函数
            getpl()
            pmtList

        /// <summary>
        /// 获取参数十六进制字符串。
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
            let pmt = this.GetParameter()

            for i = 0 to pmt.Length - 1 do
                if i < pmt.Length - 1 then
                    hexString <- (pmt.[i]).ToString("X2") + sp
                else
                    hexString <- (pmt.[i]).ToString("X2")

        /// <summary>
        /// 获取参数字符串。
        /// </summary>
        /// <returns></returns>
        override this.ToString() =
            Encoding.UTF8.GetString(this.GetParameter())