package core.parsers

import util.cache.{Cache, InfiniteCache}

// TODO misschien proberen door te parsen wanneer er een failure plaatsvindt, zodat ik bij 'missende input' gewoon verder ga. Ik zou ook nog recovery parsers kunnen toevoegen zoals in de paper, maar dat lijkt overkil.
trait Parser[Input <: ParseInput, +Result] {

  type Parser[+R] = core.parsers.Parser[Input, R]
  type ParseState = core.parsers.ParseState[Input]
  type ParseResult[+R] = core.parsers.ParseResult[Input, R]
  type ParseSuccess[+R] = core.parsers.ParseSuccess[Input, R]
  type ParseFailure[+R] = core.parsers.ParseFailure[Input, R]

  /**
    * When implementing, make sure that when returning a failure,
    * if this Parser's default has a value, then the failure must do so to.
    */
  def parseNaively(input: Input, state: ParseState): ParseResult[Result]
  def getDefault(cache: DefaultCache): Option[Result]

  def parseWholeInput(input: Input,
                      cache: Cache[ParseNode[Input], ParseResult[Any]] = new InfiniteCache()):
    ParseResult[Result] = {

    parse(input, cache)
  }

  def parse(input: Input,
            cache: Cache[ParseNode[Input], ParseResult[Any]] = new InfiniteCache()): ParseResult[Result] = {
    val state = new ParseState(cache)
    parseIteratively(input, state) match {
      case success: ParseSuccess[Result] =>
        if (success.remainder.atEnd) success
        else {
          val failedSuccess = ParseFailure(Some(success.result), success.remainder, "Did not parse entire input")
          failedSuccess.getBiggest(success.biggestFailure)
        }
      case f => f
    }
  }

  def parseCached(input: Input, state: ParseState): ParseResult[Result] = {
    val node = ParseNode(input, this)
    state.resultCache.get(node).getOrElse({
      val value = parseIteratively(input, state)
      if (!state.parsersPartOfACycle.contains(this)) {
        state.resultCache.add(node, value)
      }
      value
    }).asInstanceOf[ParseResult[Result]]
  }

  def parseIteratively(input: Input, state: ParseState): ParseResult[Result] = {
    val node = ParseNode(input, this)
    state.getPreviousResult(node) match {
      case None =>
        state.withNodeOnStack(node, () => {
          var result = parseNaively(input, state)
          result match {
            case success: ParseSuccess[Result] if state.nodesWithBackEdges.contains(this) =>
              result = growResult(node, success, state)
            case _ =>
          }
          result
        })

      case Some(result) => result
    }
  }

  private def growResult[GrowResult](node: ParseNode[Input], previous: ParseSuccess[GrowResult], state: ParseState): ParseSuccess[GrowResult] = {
    state.putIntermediate(node, previous)

    node.parser.parseNaively(node.input, state) match {
      case success: ParseSuccess[GrowResult] @unchecked if success.remainder.offset > previous.remainder.offset =>
        growResult(node, success, state)
      case _ =>
        state.removeIntermediate(node)
        previous
    }
  }

  final def getDefault(state: ParseState): Option[Result] = getDefault(state.defaultCache)

  def ~[Right](right: => Parser[Right]) = new Sequence(this, right, (a: Result,b: Right) => (a,b))
  def ~<[Right](right: Parser[Right]) = new IgnoreRight(this, right)
  def ~>[Right](right: Parser[Right]) = new IgnoreLeft(this, right)
  def |[Other >: Result](other: => Parser[Other]) = new OrElse[Input, Result, Other, Other](this, other)
  def |||[Other >: Result](other: => Parser[Other]) = new BiggestOfTwo[Input, Result, Other, Other](this, other)
  def map[NewResult](f: Result => NewResult) = new MapParser(this, f)
  def option: Parser[Option[Result]] = this.map(x => Some(x)) | Return[Input, Option[Result]](None)
  def flatMap[NewResult](f: Result => Parser[NewResult]) = new FlatMap(this, f)
  def filter[Other >: Result](predicate: Other => Boolean, getMessage: Other => String) = Filter(this, predicate, getMessage)
  def withDefault[Other >: Result](_default: Other): Parser[Other] = WithDefault[Input, Other](this, _default)

  def many[Sum](zero: Sum, reduce: (Result, Sum) => Sum) : Parser[Sum] = {
    lazy val result: Parser[Sum] = new Sequence(this, result, reduce).withDefault[Sum](zero) | Return(zero)
    result
  }

  def * : Parser[List[Result]] = {
    many(List.empty, (h: Result, t: List[Result]) => h :: t)
  }

  def ^^[NewResult](f: Result => NewResult) = new MapParser(this, f)
  def manySeparated(separator: Parser[Any]): Parser[List[Result]] =
    new Sequence(this, (separator ~> this)*, (h: Result, t: List[Result]) => h :: t) |
      Return(List.empty)
}

trait ParseInput {
  def offset: Int
  def atEnd: Boolean
}

case class ParseNode[Input <: ParseInput](input: Input, parser: Parser[Input, Any])