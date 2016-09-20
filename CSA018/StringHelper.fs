module StringHelper
open System
open System.Text
open System.Text.RegularExpressions

type String with
    /// <summary>
    /// 指示指定的字符串是 null 还是 System.String.Empty 字符串。
    /// </summary>
    /// <param name="value">要测试的字符串。</param>
    /// <returns>如果 value 参数为 null 或空字符串 ("")，则为 true；否则为 false。</returns>
    member s.IsNullOrEmpty() =
        String.IsNullOrEmpty(s)
    
    /// <summary>
    /// 指示所指定的正则表达式在指定的输入字符串中是否找到了匹配项。
    /// </summary>
    /// <param name="value">要搜索匹配项的字符串。</param>
    /// <param name="pattern">要匹配的正则表达式模式。</param>
    /// <returns>如果正则表达式找到匹配项，则为 true；否则，为 false。</returns>
    member s.IsMatch(pattern: string) =
        Regex.IsMatch(s, pattern)

    /// <summary>
    /// 判断所指定的字符串是否是十六进制形式的字符串。
    /// </summary>
    /// <param name="value">要判断的字符串。</param>
    /// <returns></returns>
    member s.IsHexString() =
        s.IsMatch("^[0-9A-Fa-f]+$")

    /// <summary>
    /// 将字符串转换为字节数组。
    /// </summary>
    /// <param name="value">要转换的字符串。</param>
    /// <param name="isHex">该字符串是否是十六进制形式,默认为false。</param>
    /// <returns></returns>
    member s.ToByteArray(?isHex: bool) =
        if not (s.IsNullOrEmpty()) then
            match isHex with
            | Some(_) ->
                let mutable s' = s.Replace(" ", "")

                if s'.StartsWith("0x", true, null) then
                    s' <- s'.Substring(2)

                if s'.IsHexString() then
                    if s'.Length % 2 > 0 then
                        s' <- "0" + s'

                    let mutable bl = []

                    for i = 0 to s'.Length / 2 do
                        bl <- bl @ [Convert.ToByte(s'.Substring(i * 2, 2), 16)]

                    List.toArray bl
                else
                    Encoding.UTF8.GetBytes(s')
            | None -> Encoding.UTF8.GetBytes(s)
        else
            [||]