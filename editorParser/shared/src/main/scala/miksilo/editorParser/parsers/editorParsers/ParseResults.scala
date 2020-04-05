package miksilo.editorParser.parsers.editorParsers

import miksilo.editorParser.parsers.core.{OffsetPointer, TextPointer}
import miksilo.editorParser.parsers.editorParsers.ParseResults._

trait ParseResults[State, +Result] extends CachingParseResult {
  def nonEmpty: Boolean
  def pop(): (LazyParseResult[State, Result], ParseResults[State, Result])
  def toList: List[LazyParseResult[State, Result]]
  def tailDepth: Int

  def merge[Other >: Result](other: ParseResults[State, Other], depth: Int = 0,
                             bests: Map[Int, Double] = Map.empty): ParseResults[State, Other]

  def map[NewResult](f: Result => NewResult): ParseResults[State, NewResult] = {
    flatMap(lazyParseResult => singleResult(lazyParseResult.map(f)), uniform = true)
  }

  def mapResult[NewResult](f: LazyParseResult[State, Result] => LazyParseResult[State, NewResult], uniform: Boolean): ParseResults[State, NewResult] = {
    flatMap(r => singleResult(f(r)), uniform)
  }

  def flatMap[NewResult](f: LazyParseResult[State, Result] => ParseResults[State, NewResult], uniform: Boolean): ParseResults[State, NewResult]

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
  def singleResult[State, Result](parseResult: LazyParseResult[State, Result]): ParseResults[State, Result] =
    new SRCons(parseResult,0, SREmpty.empty[State])
}

final class SRCons[State, +Result](
                                    val head: LazyParseResult[State, Result],
                                    var tailDepth: Int,
                                    _tail: => ParseResults[State, Result])
  extends ParseResults[State, Result] {

  override def latestRemainder: OffsetPointer = {
    head.offset
  }

  // Used for debugging
  def toList: List[LazyParseResult[State, Result]] = head :: tail.toList

  def getTail = tail
  lazy val tail = _tail

  if (tailDepth == 2) {
    tail
    tailDepth = 0
  }

  def flatMap[NewResult](f: LazyParseResult[State, Result] => ParseResults[State, NewResult],
                         uniform: Boolean): ParseResults[State, NewResult] = {
    f(head) match {
      case _: SREmpty[State] => tail.flatMap(f, uniform)
      case cons: SRCons[State, NewResult] =>

        if (!uniform && head.score != cons.head.score)
          cons.merge(tail.flatMap(f, uniform))
        else
        {
          new SRCons(
            cons.head,
            1 + Math.max(this.tailDepth, cons.tailDepth),
            cons.tail.merge(tail.flatMap(f, uniform)))
        }
      case other => other.merge(tail.flatMap(f, uniform))
    }
  }

  override def map[NewResult](f: Result => NewResult): SRCons[State, NewResult] = {
    new SRCons(head.map(f), tailDepth + 1, tail.map(f))
  }

  /*
  We don't want multiple results in the list with the same remainder. Instead we can just have the one with the best score.
  To accomplish this, we use the bests parameter.
   */
  override def merge[Other >: Result](other: ParseResults[State, Other],
                                      mergeDepth: Int,
                                      bests: Map[Int, Double] = Map.empty): ParseResults[State, Other] = {
    if (mergeDepth > 200) // Should be 200, since 100 is not enough to let CorrectionJsonTest.realLifeExample2 pass
      return SREmpty.empty[State]

    def getResult(head: LazyParseResult[State, Other], tailDepth: Int,
                  getTail: Map[Int, Double] => ParseResults[State, Other]): ParseResults[State, Other] = {
      head match {
        case ready: ReadyParseResult[State, Other] =>
          bests.get(ready.remainder.offset) match {
            case Some(previousBest) if previousBest >= ready.score =>
              getTail(bests)
            case _ =>
              new SRCons(head, tailDepth, getTail(bests + (ready.remainder.offset -> ready.score)))
          }
        case _ =>
          new SRCons(head, tailDepth, getTail(bests))
      }
    }

    other match {
      case _: SREmpty[State] => this
      case cons: SRCons[State, Other] =>
        if (head.score >= cons.head.score) {
          getResult(head, 1 + tailDepth, newBests => tail.merge(cons, mergeDepth + 1, newBests))
        } else
          getResult(cons.head, 1 + cons.tailDepth, newBests => this.merge(cons.tail, mergeDepth + 1, newBests))
      case earlier => earlier.merge(this)
    }
  }

  override def nonEmpty = true

  override def pop(): (LazyParseResult[State, Result], ParseResults[State, Result]) = (head, tail)
}

object SREmpty {
  private val value = new SREmpty[Nothing]
  def empty[State]: SREmpty[State] = value.asInstanceOf[SREmpty[State]]
}

class SREmpty[State] extends ParseResults[State, Nothing] {
  override def merge[Other >: Nothing](other: ParseResults[State, Other], depth: Int,
                                       bests: Map[Int, Double] = Map.empty) = other

  override def mapResult[NewResult](f: LazyParseResult[State, Nothing] => LazyParseResult[State, NewResult], uniform: Boolean) = this

  override def flatMap[NewResult](f: LazyParseResult[State, Nothing] => ParseResults[State, NewResult], uniform: Boolean) = this

  override def map[NewResult](f: Nothing => NewResult) = this

  override def tailDepth = 0

  override def toList = List.empty

  override def nonEmpty = false

  override def pop() = throw new Exception("Can't pop empty results")

  override def latestRemainder: OffsetPointer = EmptyRemainder
}

object EmptyRemainder extends OffsetPointer {
  override def offset() = Int.MinValue
  override def lineCharacter = Position(Int.MinValue, Int.MinValue)
}