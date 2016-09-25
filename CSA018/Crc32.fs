namespace ThisCoder.CSA018

module Crc32 =
    type Crc32() =
        static member private Table() =
            let _crc32Table = Array.zeroCreate 256
            let mutable item = 0ul

            for i = 0 to 255 do
                item <- uint32 i

                for j = 8 downto 1 do
                    if item &&& 1ul = 1ul then
                        item <- (item >>> 1) ^^^ 0xedb88320ul
                    else
                        item <- item >>> 1

                _crc32Table.[i] <- item
            _crc32Table

        /// <summary>
        /// 获取字节数组的CRC32校验值。
        /// </summary>
        /// <param name="data">需要校验的字节数组。</param>
        /// <returns></returns>
        static member GetCrc32(data: byte []) =
            let crcTable = Crc32.Table()
            let mutable crc32 = 0xfffffffful

            for item in data do
                crc32 <- (crc32 >>> 8) ^^^ crcTable.[int ((crc32 &&& 0xfful) ^^^ uint32 item)]

            crc32 ^^^ 0xfffffffful