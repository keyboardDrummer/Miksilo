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
            val maybeRightDefault = right.getDefault(state)
            if (leftSuccess.biggestFailure.offset > rightFailure.offset && maybeRightDefault.nonEmpty) {
              val rightDefault = maybeRightDefault.get
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

class IgnoreRight[Input <: ParseInput, +Result, +Right](left: Parser[Input, Result], right: Parser[Input, Right]) extends // TODO Optimize IgnoreRight and IgnoreLeft with a custom implementation
  Sequence[Input, Result, Right, Result](left, right, (l,_) => l)

class IgnoreLeft[Input <: ParseInput, +Left, +Result](left: Parser[Input, Left], right: Parser[Input, Result]) extends
  Sequence[Input, Left, Result, Result](left, right, (_,r) => r)
