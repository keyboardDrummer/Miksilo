package core.parsers

class Sequence[Input <: ParseInput, +Left, +Right, +Result](left: Parser[Input, Left], _right: => Parser[Input, Right],
                                       combine: (Left, Right) => Result) extends Parser[Input, Result] {
  lazy val right: Parser[Right] = _right

  override def parse(input: Input, state: ParseState): ParseResult[Result] = {
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

class IgnoreRight[Input <: ParseInput, +Result, +Right](left: Parser[Input, Result], right: Parser[Input, Right]) extends //TODO IgnoreRight en IgnoreLeft zouden met een custom implementatie sneller zijn.
  Sequence[Input, Result, Right, Result](left, right, (l,_) => l)

class IgnoreLeft[Input <: ParseInput, +Left, +Result](left: Parser[Input, Left], right: Parser[Input, Result]) extends
  Sequence[Input, Left, Result, Result](left, right, (_,r) => r)
