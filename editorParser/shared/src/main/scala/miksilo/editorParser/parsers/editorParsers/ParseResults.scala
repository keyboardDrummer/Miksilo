package miksilo.editorParser.parsers.editorParsers

import miksilo.editorParser.parsers.core.{OffsetPointer, TextPointer}
import miksilo.editorParser.parsers.editorParsers.ParseResults._






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
    mapResult(lazyParseResult => lazyParseResult.map(f), uniform = true)
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
      new ReadyParseResult(r.resultOption, newPosition, newState, r.history)
    }, uniform = true)
  }
}

object ParseResults {
  def singleResult[State, Result](parseResult: LazyParseResult[State, Result]): ParseResults[State, Result] = {
    parseResult match {
      case ready: ReadyParseResult[State, Result] =>
        ReadyResults(Map(ready.remainder.offset -> ready), new NilDelayed[State])
      case delayed: DelayedParseResult[State, Result] =>
        new PairingNode(delayed)
    }
  }

  def singleResult3[State, Result](parseResult: LazyParseResult[State, Result]): ParseResults[State, Result] = {
    parseResult match {
      case ready: ReadyParseResult[State, Result] =>
        ReadyResults(Map(ready.remainder.offset -> ready), new NilDelayed[State])
      case delayed: DelayedParseResult[State, Result] =>
        new ConsDelayed(delayed, new NilDelayed[State])
    }
  }

  def singleResult2[State, Result](parseResult: LazyParseResult[State, Result]): ParseResults[State, Result] = {
    parseResult match {
      case ready: ReadyParseResult[State, Result] =>
        ReadyResults(Map(ready.remainder.offset -> ready),
          new SortedDelayedList(List.empty))
      case delayed: DelayedParseResult[State, Result] =>
        new SortedDelayedList(List(delayed))
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