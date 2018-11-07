package core.parsers

trait CommonParsers extends RegexParsers {

//  def ident: Parser[String] =
//      Some(acceptIf(Character.isJavaIdentifierStart)("identifier expected but `" + _ + "' found"),
//        elem("identifier part", Character.isJavaIdentifierPart(_: Char))) ^^ (_.mkString)
//    )

  /** An integer, without sign or with a negative sign. */
  def wholeNumber: Parser[String] =
    """-?\d+""".r
  /** Number following one of these rules:
    *
    *  - An integer. For example: `13`
    *  - An integer followed by a decimal point. For example: `3.`
    *  - An integer followed by a decimal point and fractional part. For example: `3.14`
    *  - A decimal point followed by a fractional part. For example: `.1`
    */
  def decimalNumber: Parser[String] =
    """(\d+(\.\d*)?|\d*\.\d+)""".r
  /** Double quotes (`"`) enclosing a sequence of:
    *
    *  - Any character except double quotes, control characters or backslash (`\`)
    *  - A backslash followed by another backslash, a single or double quote, or one
    *    of the letters `b`, `f`, `n`, `r` or `t`
    *  - `\` followed by `u` followed by four hexadecimal digits
    */
  def stringLiteral: Parser[String] =
    ("\""+"""([^"\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*"""+"\"").r
  /** A number following the rules of `decimalNumber`, with the following
    *  optional additions:
    *
    *  - Preceded by a negative sign
    *  - Followed by `e` or `E` and an optionally signed integer
    *  - Followed by `f`, `f`, `d` or `D` (after the above rule, if both are used)
    */
  def floatingPointNumber: Parser[String] =
    """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r
}
