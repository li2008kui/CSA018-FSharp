namespace ThisCoder.CSA018
open System.IO
open System.Security.Cryptography
open System.Text

module DESHelper =
    /// <summary>
    /// DES 助手类。
    /// <para>运算模式为 ECB 模式。</para>
    /// </summary>
    type DESHelper() =
        /// <summary>
        /// 获取 DES 密钥。
        /// </summary>
        /// <returns>DES 密钥。</returns>
        member this.GetSecretKey() =
            let mutable secretKey = [||]
            let mutable des: DESCryptoServiceProvider = null

            try
                des <- new DESCryptoServiceProvider()
                des.Mode <- CipherMode.ECB
                secretKey <- des.Key
            finally
                des.Dispose()

            secretKey

        /// <summary>
        /// 使用密钥加密明文字节数组。
        /// </summary>
        /// <param name="keyBytes">DES 密钥字节数组。</param>
        /// <param name="plainBytes">需要加密的字节数组。</param>
        /// <returns>密文字节数组。</returns>
        member this.Encrypt(keyBytes: byte [], plainBytes: byte []) =
            let mutable cipherBytes = [||]
            let mutable des: DESCryptoServiceProvider = null
            let mutable ms: MemoryStream = null
            let mutable cs: CryptoStream = null
            let mutable sw: StreamWriter = null

            try
                des <- new DESCryptoServiceProvider()
                des.Mode <- CipherMode.ECB
                des.Key <- keyBytes

                ms <- new MemoryStream()
                cs <- new CryptoStream(ms, des.CreateDecryptor(), CryptoStreamMode.Write)
                sw <- new StreamWriter(cs)
                sw.WriteLine(Encoding.ASCII.GetChars(plainBytes))
                cipherBytes <- ms.ToArray()
            finally
                sw.Dispose()
                ms.Dispose()
                ms.Dispose()
                des.Dispose()

            cipherBytes

        /// <summary>
        /// 使用密钥加密明文字符串。
        /// </summary>
        /// <param name="keyBytes">DES 密钥字节数组。</param>
        /// <param name="plainText">需要加密的字符串。</param>
        /// <returns></returns>
        member this.Encrypt(keyBytes: byte [], plainText: string) =
            this.Encrypt(keyBytes, Encoding.UTF8.GetBytes(plainText))

        /// <summary>
        /// 使用密钥解密密文字节数组。
        /// </summary>
        /// <param name="keyBytes">DES 密钥字节数组。</param>
        /// <param name="cipherBytes">需要解密的字节数组。</param>
        /// <returns>明文字节数组。</returns>
        member this.Decrypt(keyBytes: byte [], cipherBytes: byte []) =
            let mutable plainBytes = [||]
            let mutable des: DESCryptoServiceProvider = null
            let mutable ms: MemoryStream = null
            let mutable cs: CryptoStream = null
            let mutable sr: StreamReader = null

            try
                des <- new DESCryptoServiceProvider()
                des.Mode <- CipherMode.ECB
                des.Key <- keyBytes

                ms <- new MemoryStream()
                cs <- new CryptoStream(ms, des.CreateDecryptor(), CryptoStreamMode.Read)
                sr <- new StreamReader(cs)
                plainBytes <- Encoding.UTF8.GetBytes(sr.ReadLine())
            finally
                sr.Dispose()
                ms.Dispose()
                ms.Dispose()
                des.Dispose()

            plainBytes

        /// <summary>
        /// 使用密钥解密密文字符串。
        /// </summary>
        /// <param name="keyBytes">DES 密钥字节数组。</param>
        /// <param name="cipherText">需要解密的字符串。</param>
        /// <returns>明文字节数组。</returns>
        member this.Decrypt(keyBytes: byte [], cipherText: string) =
            this.Decrypt(keyBytes, Encoding.UTF8.GetBytes(cipherText))