package core.parsers

//class MapParser[Input <: ParseInput, +Result, NewResult](original: Parser[Input, Result], f: Result => NewResult) extends Parser[Input, NewResult] {
//  override def parseNaively(input: Input, cache: ParseState): ParseResult[NewResult] = {
//    original.parseCached(input, cache).map(f)
//  }
//
//  override def getDefault(cache: DefaultCache): Option[NewResult] = cache(original).map(f)
//}
