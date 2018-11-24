package core.parsers

// TODO consider replacing this with FlatMap[Input, Left, Result](left, l => _right.map(r => combine(l, r))) although for some reason that currently reduces the error recovery
class Sequence[Input <: ParseInput, +Left, +Right, +Result](left: Parser[Input, Left],
                                                            _right: => Parser[Input, Right],
                                       combine: (Left, Right) => Result) extends Parser[Input, Result] {
  lazy val right: Parser[Right] = _right

  override def parseNaively(input: Input, state: ParseState): ParseResult[Result] = {
    val leftResult = left.parseCached(input, state)
    leftResult match {
      case leftSuccess: ParseSuccess[Left] =>
        val rightResult = right.parseCached(leftSuccess.remainder, state)
        rightResult match {
          case rightSuccess: ParseSuccess[Right] =>
            rightSuccess.map(r => combine(leftSuccess.result, r)).
              addFailure(leftSuccess.biggestFailure.map(l => combine(l, rightSuccess.result)))

          case rightFailure: ParseFailure[Right] =>
            if (leftSuccess.biggestFailure.offset > rightFailure.offset && rightFailure.partialResult.nonEmpty) {
              val rightDefault = rightFailure.partialResult.get
              leftSuccess.biggestFailure.map(l => combine(l, rightDefault)).asInstanceOf[ParseFailure[Result]]
            }
            else {
              rightFailure.map(right => combine(leftSuccess.result, right))
            }
        }

      case leftFailure: ParseFailure[Left] =>
        val result = for {
          leftPartial <- leftFailure.partialResult
          rightDefault <- right.getDefault(state)
        } yield combine(leftPartial, rightDefault)
        ParseFailure(result, leftFailure.remainder, leftFailure.message)
    }
  }

  override def getDefault(cache: DefaultCache): Option[Result] = for {
    leftDefault <- cache(left)
    rightDefault <- cache(right)
  } yield combine(leftDefault, rightDefault)
}
