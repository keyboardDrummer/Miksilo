package miksilo.editorParser.parsers.editorParsers

import miksilo.editorParser.parsers.core.{OffsetPointer, TextPointer}
import miksilo.editorParser.parsers.editorParsers.ParseResults._

trait DelayedResults[State, +Result] extends ParseResults[State, Result] {

  override def mapWithHistory[NewResult](f: ReadyParseResult[State, Result] => ReadyParseResult[State, NewResult],
                                         oldHistory: History): ParseResults[State, NewResult] = {
    mapResult(l => l.mapWithHistory(f, oldHistory), uniform = !oldHistory.canMerge)
  }

  override def flatMap[NewResult](f: LazyParseResult[State, Result] => ParseResults[State, NewResult],
                                  uniform: Boolean): DelayedResults[State, NewResult] = {
    new FlatMapDelayed(this, f)
  }

  override def merge[Other >: Result](other: ParseResults[State, Other]): ParseResults[State, Other] = {
    other match {
      case mergeDelayedList: MergeDelayedList[State, Result] => mergeDelayedList.merge(this)
      case delayedOther: DelayedResults[State, Result] => new MergeDelayedList(List(this, delayedOther))
      case _ => other.merge(this)
    }

  }

  override def toList: List[LazyParseResult[State, Result]] = ???

  override def latestRemainder: OffsetPointer = ???
}

class ConsDelayed[State, +Result](head: LazyParseResult[State, Result], tail: DelayedResults[State, Result])
  extends DelayedResults[State, Result] {

  override def pop(): Option[(LazyParseResult[State, Result], ParseResults[State, Result])] = {
    Some(head, tail)
  }
}

class NilDelayed[State] extends DelayedResults[State, Nothing] {
  override def pop(): Option[(LazyParseResult[State, Nothing], DelayedResults[State, Nothing])] = None
}

class MergeDelayedList[State, +Result](val options: List[DelayedResults[State, Result]])
  extends DelayedResults[State, Result] {

  override def merge[Other >: Result](other: ParseResults[State, Other]): ParseResults[State, Other] = {
    other match {
      case mergeDelayedList: MergeDelayedList[State, Result] => new MergeDelayedList(options ++ mergeDelayedList.options)
      case delayedOther: DelayedResults[State, Result] => new MergeDelayedList(delayedOther :: options)
      case _ => other.merge(this)
    }

  }

  override def pop(): Option[(LazyParseResult[State, Result], ParseResults[State, Result])] = {
    val pops = options.flatMap(o => o.pop().toSeq)
    if (pops.isEmpty)
      return None

    val maxPop = pops.maxBy(p => p._1.score)
    val otherPops = pops.filter(p => p != maxPop).map(t => singleResult(t._1).merge(t._2)).fold(maxPop._2)((a,b) => a.merge(b))
    Some(maxPop._1, otherPops)
  }
}

class MergeDelayed[State, +Result](first: DelayedResults[State, Result], second: DelayedResults[State, Result])
  extends DelayedResults[State, Result] {

  override def pop(): Option[(LazyParseResult[State, Result], ParseResults[State, Result])] = {
    val firstPop = first.pop()
    val secondPop = second.pop()
    (firstPop, secondPop) match {
      case (None, _) => secondPop
      case (_, None) => firstPop
      case (Some((firstHead, firstTail)), Some((secondHead, secondTail))) =>
        if (firstHead.score > secondHead.score) {
          Some(firstHead, firstTail.merge(singleResult(secondHead).merge(secondTail)))
        } else {
          Some(secondHead, secondTail.merge(singleResult(firstHead).merge(firstTail)))
        }
    }
  }
}

class FlatMapDelayed[State, Result, +NewResult](original: DelayedResults[State, Result], f: LazyParseResult[State, Result] => ParseResults[State, NewResult])
  extends DelayedResults[State, NewResult] {

  override def pop(): Option[(LazyParseResult[State, NewResult], ParseResults[State, NewResult])] = {
    original.pop().flatMap(originalPop => {
      val applied = f(originalPop._1)
      val appliedPop = applied.pop()
      appliedPop.map(p => (p._1, p._2.merge(originalPop._2.flatMap(f, uniform = true))))
    })
  }
}

case class ReadyResults[State, +Result](ready: Map[Int, ReadyParseResult[State, Result]],
                                        delayed: DelayedResults[State, Result]) extends ParseResults[State, Result] {
  override def pop(): Option[(LazyParseResult[State, Result], ParseResults[State, Result])] = {
    if (ready.nonEmpty) {
      val head = ready.head
      Some((head._2, ReadyResults(ready.removed(head._1), delayed)))
    } else {
      delayed.pop()
    }
  }

  def mergeMaps[Key, Value](first: Map[Key, Value], second: Map[Key, Value],
                            combine: (Key, Value, Value) => Value): Map[Key, Value] = {
    var result: Map[Key, Value] = first
    for((secondKey, secondValue) <- second) {
      result += (secondKey -> (first.get(secondKey) match {
        case None => second(secondKey)
        case Some(firstValue) => combine(secondKey, firstValue, secondValue)
      }))
    }
    result
  }

  override def merge[Other >: Result](other: ParseResults[State, Other]): ParseResults[State, Other] = {
    other match {
      case otherReady: ReadyResults[State, Result] =>
        val mergedReady = mergeMaps[Int, ReadyParseResult[State, Result]](ready, otherReady.ready, (key, value1, value2) => {
          if (value1.score > value2.score) value1 else value2
        })
        ReadyResults(mergedReady, delayed.merge(otherReady.delayed).asInstanceOf[DelayedResults[State, Result]])
      case delayedOther: DelayedResults[State, Result] =>
        ReadyResults(ready, delayed.merge(delayedOther).asInstanceOf[DelayedResults[State, Result]])
      case _ => other.merge(this)
    }
  }

  override def flatMap[NewResult](f: LazyParseResult[State, Result] => ParseResults[State, NewResult],
                                  uniform: Boolean): ParseResults[State, NewResult] = {
    ready.values.map(ready => f(ready)).fold(delayed.flatMap(f, uniform))((a,b) => a.merge(b))
  }

  override def toList: List[LazyParseResult[State, Result]] = ???

  override def latestRemainder: OffsetPointer = ???
}

/**
  * A collection of paths in the parse graph.
  * The parse graph is the graph containing all triples of offset, parser and context as nodes.
  * Paths can be partial, meaning we can still continue parsing from the end of the path.
  * Path are sorted according to their score. The score indicates how likely this path is what the writer intended.
  */
trait ParseResults[State, +Result] extends CachingParseResult {

  def pop(): Option[(LazyParseResult[State, Result], ParseResults[State, Result])]

  def toList: List[LazyParseResult[State, Result]]

  def merge[Other >: Result](other: ParseResults[State, Other]): ParseResults[State, Other]

  def map[NewResult](f: Result => NewResult): ParseResults[State, NewResult] = {
    flatMap(lazyParseResult => singleResult(lazyParseResult.map(f)), uniform = true)
  }

  def mapResult[NewResult](f: LazyParseResult[State, Result] => LazyParseResult[State, NewResult], uniform: Boolean): ParseResults[State, NewResult] = {
    flatMap(r => singleResult(f(r)), uniform)
  }

  def flatMap[NewResult](f: LazyParseResult[State, Result] => ParseResults[State, NewResult],
                         uniform: Boolean): ParseResults[State, NewResult]

  def addHistory(errors: History): ParseResults[State, Result] = {
    mapWithHistory(x => x, errors)
  }

  def mapWithHistory[NewResult](f: ReadyParseResult[State, Result] => ReadyParseResult[State, NewResult],
                                oldHistory: History): ParseResults[State, NewResult] = {
    mapResult(l => l.mapWithHistory(f, oldHistory), uniform = !oldHistory.canMerge)
  }

  def mapReady[NewResult](f: ReadyParseResult[State, Result] => ReadyParseResult[State, NewResult],
                          uniform: Boolean): ParseResults[State, NewResult] = {
    mapResult(l => l.mapReady(f, uniform), uniform)
  }

  def flatMapReady[NewResult](f: ReadyParseResult[State, Result] => ParseResults[State, NewResult], uniform: Boolean): ParseResults[State, NewResult] = {
    flatMap[NewResult](l => l.flatMapReady(f, uniform), uniform)
  }

  def updateRemainder(f: (TextPointer, State) => (TextPointer, State)): ParseResults[State, Result] = {
    mapReady(r => {
      val (newPosition, newState) = f(r.remainder, r.state)
      ReadyParseResult(r.resultOption, newPosition, newState, r.history)
    }, uniform = true)
  }
}

object ParseResults {
  def singleResult[State, Result](parseResult: LazyParseResult[State, Result]): ParseResults[State, Result] = {
    parseResult match {
      case ready: ReadyParseResult[State, Result] =>
        ReadyResults(Map(ready.remainder.offset -> ready), new NilDelayed())
      case delayed: DelayedParseResult[State, Result] =>
        new ConsDelayed(delayed, new NilDelayed[State])
    }
  }
}

object SREmpty {
  private val value = new SREmpty[Nothing]
  def empty[State]: SREmpty[State] = value.asInstanceOf[SREmpty[State]]
}

class SREmpty[State] extends ParseResults[State, Nothing] {
  override def merge[Other >: Nothing](other: ParseResults[State, Other]): ParseResults[State, Other] = {
      other
  }

  override def mapResult[NewResult](f: LazyParseResult[State, Nothing] => LazyParseResult[State, NewResult], uniform: Boolean) = this

  override def flatMap[NewResult](f: LazyParseResult[State, Nothing] => ParseResults[State, NewResult],
                                  uniform: Boolean) = this

  override def map[NewResult](f: Nothing => NewResult) = this

  override def toList = List.empty

  override def latestRemainder: OffsetPointer = EmptyRemainder

  override def pop(): Option[(LazyParseResult[State, Nothing], ParseResults[State, Nothing])] = None
}

object EmptyRemainder extends OffsetPointer {
  override def offset(): Int = Int.MinValue
  override def lineCharacter: Position = Position(Int.MinValue, Int.MinValue)
}