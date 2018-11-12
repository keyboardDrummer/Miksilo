package core.parsers

//TODO add tests
class FlatMap[Input <: ParseInput, +Result, +NewResult](left: Parser[Input, Result],
                                                        getRight: Result => Parser[Input, NewResult])
  extends Parser[Input, NewResult] {

  override def parseNaively(input: Input, state: ParseState): ParseResult[NewResult] = {
    val leftResult = left.parseCached(input, state)
    leftResult match {
      case leftSuccess: ParseSuccess[Result] =>
        val right = getRight(leftSuccess.result)
        val rightResult = right.parseCached(leftSuccess.remainder, state)
        rightResult match {
          case rightSuccess: ParseSuccess[NewResult] =>
            rightSuccess.
              addFailure(leftSuccess.biggestFailure match {
                case NoFailure => NoFailure
                case ParseFailure(partialResult, remainder, message) =>
                  ParseFailure(partialResult.flatMap(leftPartial => getRight(leftPartial).getDefault(state)), remainder, message)
              })

          case rightFailure: ParseFailure[NewResult] =>
            if (leftSuccess.biggestFailure.offset > rightFailure.offset) {
              val biggestFailure = leftSuccess.biggestFailure.asInstanceOf[ParseFailure[Result]]
              ParseFailure(rightFailure.partialResult, biggestFailure.remainder, biggestFailure.message)
            }
            else {
              rightFailure
            }
        }

      case leftFailure: ParseFailure[Result] =>
        val result = for {
          leftPartial <- leftFailure.partialResult
          rightDefault <- getRight(leftPartial).getDefault(state)
        } yield rightDefault
        ParseFailure(result, leftFailure.remainder, leftFailure.message)
    }
  }

  override def getDefault(cache: DefaultCache): Option[NewResult] = for {
    leftDefault <- cache(left)
    rightDefault <- cache(getRight(leftDefault))
  } yield rightDefault
}
