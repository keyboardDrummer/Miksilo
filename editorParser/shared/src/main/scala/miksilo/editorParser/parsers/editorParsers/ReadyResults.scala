package miksilo.editorParser.parsers.editorParsers

import miksilo.editorParser.parsers.core.OffsetPointer

case class ReadyResults[State, +Result](ready: List[ReadyParseResult[State, Result]],
                                        delayed: ParseResultsWithDelayed[State, Result])
  extends ParseResults[State, Result] {

  def this(result: ReadyParseResult[State, Result]) = {
    this(List(result), SREmpty.empty[State])
  }

  override def pop(): Option[(LazyParseResult[State, Result], ParseResults[State, Result])] = {
    if (ready.nonEmpty) {
      val head = ready.head
      val remaining = ready.tail
      Some((head, if (remaining.nonEmpty) ReadyResults(remaining, delayed) else delayed))
    } else {
      delayed.pop()
    }
  }

  override def merge[Other >: Result](other: ParseResults[State, Other], remainingListLength: Int):
    ParseResults[State, Other] = {
    other match {
      case otherReady: ReadyResults[State, Result] =>
        ReadyResults(ready ++ otherReady.ready, delayed.merge(otherReady.delayed, remainingListLength).asInstanceOf[ParseResultsWithDelayed[State, Result]])
      case delayedOther: SRCons[State, Result] =>
        ReadyResults(ready, delayed.merge(delayedOther, remainingListLength).asInstanceOf[ParseResultsWithDelayed[State, Result]])
      case _ => other.merge(this, remainingListLength)
    }
  }

  override def flatMap[NewResult](f: LazyParseResult[State, Result] => ParseResults[State, NewResult],
                                  uniform: Boolean,
                                  remainingListLength: Int): ParseResults[State, NewResult] = {
    val newDelayed = delayed.flatMap(f, uniform, remainingListLength)
    ready.map(ready => f(ready)).fold(newDelayed)((a, b) => a.merge(b, remainingListLength))
  }

  override def toList: List[LazyParseResult[State, Result]] = {
    val delayedList = delayed.toList
    ready ++ delayedList
  }

  override def latestRemainder: OffsetPointer = {
    OffsetPointer.ordering.max(ready.map(r => r.remainder).max(OffsetPointer.ordering),
      delayed.latestRemainder)
  }

  override def tailDepth: Int = delayed.tailDepth

  override def containsStrictParts: Boolean = true
}

case class ReadyResults2[State, +Result](ready: Map[Int, ReadyParseResult[State, Result]],
                                        delayed: ParseResultsWithDelayed[State, Result])
  extends ParseResults[State, Result] {

  def this(result: ReadyParseResult[State, Result]) = {
    this(Map(result.offset.offset -> result), SREmpty.empty[State])
  }

  override def pop(): Option[(LazyParseResult[State, Result], ParseResults[State, Result])] = {
    if (ready.nonEmpty) {
      val head = ready.head
      val remaining = ready.removed(head._1)
      Some((head._2, if (remaining.nonEmpty) ReadyResults2(remaining, delayed) else delayed))
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

  override def merge[Other >: Result](other: ParseResults[State, Other], remainingListLength: Int):
  ParseResults[State, Other] = {
    other match {
      case otherReady: ReadyResults2[State, Result] =>
        val mergedReady = mergeMaps[Int, ReadyParseResult[State, Result]](ready, otherReady.ready, (_, value1, value2) => {
          if (value1.score > value2.score) value1 else value2
        })
        ReadyResults2(mergedReady, delayed.merge(otherReady.delayed, remainingListLength).asInstanceOf[ParseResultsWithDelayed[State, Result]])
      case delayedOther: SRCons[State, Result] =>
        ReadyResults2(ready, delayed.merge(delayedOther, remainingListLength).asInstanceOf[ParseResultsWithDelayed[State, Result]])
      case _ => other.merge(this, remainingListLength)
    }
  }

  override def flatMap[NewResult](f: LazyParseResult[State, Result] => ParseResults[State, NewResult],
                                  uniform: Boolean,
                                  remainingListLength: Int): ParseResults[State, NewResult] = {
    val newDelayed = delayed.flatMap(f, uniform, remainingListLength)
    ready.values.map(ready => f(ready)).fold(newDelayed)((a, b) => a.merge(b, remainingListLength))
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

  override def containsStrictParts: Boolean = true
}