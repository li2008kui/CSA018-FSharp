namespace ThisCoder.CSA018
open System
open System.Linq
open System.Text

module BytesHelper =
    type ByteArray =
        static member ToHexString(value: byte [], ?separator: string) =
            let mutable sp = " "

            if separator.IsSome then
                sp <- separator.Value

            String.Join(sp, value.Select(fun b -> b.ToString("X2")))

        static member ToString2(value: byte []) =
            Encoding.UTF8.GetString(value)