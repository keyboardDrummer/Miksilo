package miksilo.editorParser.parsers.editorParsers

import miksilo.editorParser.parsers.core.OffsetPointer

case class ReadyResults[State, +Result](ready: Map[Int, ReadyParseResult[State, Result]],
                                        delayed: ParseResultsWithDelayed[State, Result]) extends ParseResults[State, Result] {

  override def pop(): Option[(LazyParseResult[State, Result], ParseResults[State, Result])] = {
    if (ready.nonEmpty) {
      val head = ready.head
      val remaining = ready.removed(head._1)
      Some((head._2, if (remaining.nonEmpty) ReadyResults(ready.removed(head._1), delayed) else delayed))
    } else {
      delayed.pop()
    }
  }

  def mergeMaps[Key, Value](first: Map[Key, Value], second: Map[Key, Value],
                            combine: (Key, Value, Value) => Value): Map[Key, Value] = {
    var result: Map[Key, Value] = first
    for ((secondKey, secondValue) <- second) {
      result += (secondKey -> (first.get(secondKey) match {
        case None => second(secondKey)
        case Some(firstValue) => combine(secondKey, firstValue, secondValue)
      }))
    }
    result
  }

  override def merge[Other >: Result](other: ParseResults[State, Other], remainingListLength: Int): ParseResults[State, Other] = {
    other match {
      case otherReady: ReadyResults[State, Result] =>
        val mergedReady = mergeMaps[Int, ReadyParseResult[State, Result]](ready, otherReady.ready, (key, value1, value2) => {
          if (value1.score > value2.score) value1 else value2
        })
        ReadyResults(mergedReady, delayed.merge(otherReady.delayed, remainingListLength).asInstanceOf[ParseResultsWithDelayed[State, Result]])
      case delayedOther: SRCons[State, Result] =>
        ReadyResults(ready, delayed.merge(delayedOther, remainingListLength).asInstanceOf[ParseResultsWithDelayed[State, Result]])
      case _ => other.merge(this, remainingListLength)
    }
  }

  override def flatMap[NewResult](f: LazyParseResult[State, Result] => ParseResults[State, NewResult],
                                  uniform: Boolean,
                                  remainingListLength: Int): ParseResults[State, NewResult] = {
    ready.values.map(ready => f(ready)).fold(delayed.flatMap(f, uniform, remainingListLength))((a, b) => a.merge(b, remainingListLength))
  }

  override def toList: List[LazyParseResult[State, Result]] = {
    val delayedList = delayed.toList
    ready.values.foldLeft(delayedList)((a, b) => b :: a)
  }

  override def latestRemainder: OffsetPointer = {
    OffsetPointer.ordering.max(ready.values.map(r => r.remainder).max(OffsetPointer.ordering),
      delayed.latestRemainder)
  }

  override def tailDepth: Int = delayed.tailDepth
}