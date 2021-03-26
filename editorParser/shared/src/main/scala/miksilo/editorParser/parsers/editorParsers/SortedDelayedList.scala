package miksilo.editorParser.parsers.editorParsers

import miksilo.editorParser.parsers.core.OffsetPointer

class SortedDelayedList[State, Result](val sortedItems: List[DelayedParseResult[State, Result]])
  extends DelayedResults[State, Result] {

  // Minimum length required to make "starting brace insertion unambiguous" pass
  val maxLength = 17

  override def toList: List[LazyParseResult[State, Result]] = {
    sortedItems
  }

  override def merge[Other >: Result](other: ParseResults[State, Other]): ParseResults[State, Other] = {
    other match {
      case delayedList: SortedDelayedList[State, Result] =>
        var reverseSorted = List.empty[DelayedParseResult[State, Result]]
        var leftRemainder = sortedItems
        var rightRemainder = delayedList.sortedItems
        var count = 0
        while(count < maxLength && leftRemainder.nonEmpty && rightRemainder.nonEmpty) {
          if (leftRemainder.head.score > rightRemainder.head.score) {
            reverseSorted ::= leftRemainder.head
            leftRemainder = leftRemainder.tail
          } else {
            reverseSorted ::= rightRemainder.head
            rightRemainder = rightRemainder.tail
          }
          count += 1
        }
        val sorted = if (count == maxLength) {
          reverseSorted.reverse
        } else {
          reverseSorted.reverse ++ rightRemainder ++ leftRemainder
        }
        new SortedDelayedList(sorted)
      case _ => other.merge(this)
    }

  }

  override def pop(): Option[(DelayedParseResult[State, Result], DelayedResults[State, Result])] = {
    sortedItems match {
      case Nil => None
      case head :: tail => Some((head, new SortedDelayedList(tail)))
    }
  }

  override def flatMap[NewResult](f: LazyParseResult[State, Result] => ParseResults[State, NewResult]): ParseResults[State, NewResult] = {
    sortedItems.map(d => f(d)).fold(SREmpty.empty[State])((a, b) => a.merge(b))
  }

  override def latestRemainder: OffsetPointer = sortedItems.map(d => d.initialOffset).
    maxOption(OffsetPointer.ordering).getOrElse(EmptyRemainder)
}
