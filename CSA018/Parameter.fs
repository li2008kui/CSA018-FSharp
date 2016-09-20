namespace ThisCoder.CSA018
open System.Text
open ParameterType

module Parameter =
    type Parameter(type': ParameterType, value: byte[]) =
        let mutable _type = type'
        let mutable _value = value
        new (type': ParameterType, value: string) =
            Parameter(type', Encoding.UTF8.GetBytes(value))
        new (type': ParameterType, value: byte) =
            Parameter(type', [|value|])
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