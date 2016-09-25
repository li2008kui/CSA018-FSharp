namespace ThisCoder.CSA018
open System
open System.Runtime.Serialization
open ErrorCode

module CsaException =
    /// <summary>
    /// Initializes a new instance of the CsaException class with
    /// a specified error message and a reference to the inner exception that is
    /// the cause of this exception.
    /// </summary>
    /// <param name="message">解释异常原因的错误信息。</param>
    /// <param name="innerException">导致当前异常的异常。如果 innerException 参数不为空引用，则在处理内部异常的 catch 块中引发当前异常。</param>
    /// <param name="code">
    /// 错误代码。
    /// <para><see cref="ErrorCode"/>类型，长度为4个字节。</para>
    /// </param>
    type CsaException(message: string, innerException: Exception, ?code: ErrorCode) =
        inherit ApplicationException(message, innerException)

        let mutable _errorCode =
            match code with
            | Some(_) -> code.Value
            | None -> ErrorCode.Unknown

        /// <summary>
        /// 初始化 CsaException 类的新实例。
        /// </summary>
        /// <param name="code">
        /// 错误代码。
        /// <para><see cref="ErrorCode"/>类型，长度为4个字节。</para>
        /// </param>
        new (?code: ErrorCode) =
            match code with
            | Some(_) ->
                CsaException(null, code.Value)
            | None -> CsaException(null, ErrorCode.Unknown)

        /// <summary>
        /// 使用指定错误消息初始化 CsaException 类的新实例。
        /// </summary>
        /// <param name="message">解释异常原因的错误信息。</param>
        /// <param name="code">
        /// 错误代码。
        /// <para><see cref="ErrorCode"/>类型，长度为4个字节。</para>
        /// </param>
        new (message: string, ?code: ErrorCode) =
            match code with
            | Some(_) ->
                CsaException(message, null, code.Value)
            | None -> CsaException(message, null, ErrorCode.Unknown)

        /// <summary>
        /// 错误代码。
        /// <para><see cref="ErrorCode"/>类型，长度为4个字节。</para>
        /// </summary>
        member this.ErrorCode
            with get() = _errorCode
            and private set e = _errorCode <- e
