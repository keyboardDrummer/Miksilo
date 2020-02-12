package core.parsers.editorParsers

import ParseResults._
import core.parsers.core.ParseInput

trait ParseResults[Input <: ParseInput[Input], +Result] {
  def latestRemainder: Int
  def nonEmpty: Boolean
  def pop(): (LazyParseResult[Input, Result], ParseResults[Input, Result])
  def toList: List[LazyParseResult[Input, Result]]
  def tailDepth: Int

  def move(array: ArrayCharSequence, offset: Int): ParseResults[Input, Result]

  def merge[Other >: Result](other: ParseResults[Input, Other], depth: Int = 0,
                             bests: Map[Input, Double] = Map.empty): ParseResults[Input, Other]

  def map[NewResult](f: Result => NewResult): ParseResults[Input, NewResult] = {
    flatMap(lazyParseResult => singleResult(lazyParseResult.map(f)), uniform = true)
  }

  def mapResult[NewResult](f: LazyParseResult[Input, Result] => LazyParseResult[Input, NewResult], uniform: Boolean): ParseResults[Input, NewResult] = {
    flatMap(r => singleResult(f(r)), uniform)
  }

  def flatMap[NewResult](f: LazyParseResult[Input, Result] => ParseResults[Input, NewResult], uniform: Boolean): ParseResults[Input, NewResult]

  def addHistory(errors: History[Input]): ParseResults[Input, Result] = {
    mapWithHistory(x => x, errors)
  }

  def mapWithHistory[NewResult](f: ReadyParseResult[Input, Result] => ReadyParseResult[Input, NewResult],
                                oldHistory: History[Input]): ParseResults[Input, NewResult] = {
    mapResult(l => l.mapWithHistory(f, oldHistory), uniform = !oldHistory.canMerge)
  }

  def mapReady[NewResult](f: ReadyParseResult[Input, Result] => ReadyParseResult[Input, NewResult],
                          uniform: Boolean): ParseResults[Input, NewResult] = {
    mapResult(l => l.mapReady(f, uniform), uniform)
  }

  def flatMapReady[NewResult](f: ReadyParseResult[Input, Result] => ParseResults[Input, NewResult], uniform: Boolean): ParseResults[Input, NewResult] = {
    flatMap[NewResult](l => l.flatMapReady(f, uniform), uniform)
  }

  def updateRemainder(f: Input => Input): ParseResults[Input, Result] = {
    mapReady(r => ReadyParseResult(r.resultOption, f(r.remainder), r.history), uniform = true)
  }
}

object ParseResults {
  def singleResult[Input <: ParseInput[Input], Result](parseResult: LazyParseResult[Input, Result]): ParseResults[Input, Result] =
    new SRCons(parseResult, parseResult.offset,0, SREmpty.empty[Input])
}

final class SRCons[Input <: ParseInput[Input], +Result](
  val head: LazyParseResult[Input, Result],
  val latestRemainder: Int,
  var tailDepth: Int,
  _tail: => ParseResults[Input, Result])
  extends ParseResults[Input, Result] {

  // Used for debugging
  def toList: List[LazyParseResult[Input, Result]] = head :: tail.toList

  def getTail = tail
  lazy val tail = _tail

  if (tailDepth == 50) {
    tail
    tailDepth = 0
  }

  def flatMap[NewResult](f: LazyParseResult[Input, Result] => ParseResults[Input, NewResult],
                         uniform: Boolean): ParseResults[Input, NewResult] = {
    f(head) match {
      case _: SREmpty[Input] => tail.flatMap(f, uniform)
      case cons: SRCons[Input, NewResult] =>

        if (!uniform && head.score != cons.head.score)
          cons.merge(tail.flatMap(f, uniform))
        else
        {
          new SRCons(
            cons.head,
            cons.latestRemainder, // TODO is this correct?
            1 + Math.max(this.tailDepth, cons.tailDepth),
            cons.tail.merge(tail.flatMap(f, uniform)))
        }
      case other => other.merge(tail.flatMap(f, uniform))
    }
  }

  override def map[NewResult](f: Result => NewResult): SRCons[Input, NewResult] = {
    new SRCons(head.map(f), latestRemainder, tailDepth + 1, tail.map(f))
  }

  /*
  We don't want multiple results in the list with the same remainder. Instead we can just have the one with the best score.
  To accomplish this, we use the bests parameter.
   */
  override def merge[Other >: Result](other: ParseResults[Input, Other],
                                      mergeDepth: Int,
                                      bests: Map[Input, Double] = Map.empty): ParseResults[Input, Other] = {
    if (mergeDepth > 200) // Should be 200, since 100 is not enough to let CorrectionJsonTest.realLifeExample2 pass
      return SREmpty.empty[Input]

    def getResult(head: LazyParseResult[Input, Other], latestRemainder: Int, tailDepth: Int,
                  getTail: Map[Input, Double] => ParseResults[Input, Other]): ParseResults[Input, Other] = {
      head match {
        case ready: ReadyParseResult[Input, Other] =>
          bests.get(ready.remainder) match {
            case Some(previousBest) if previousBest >= ready.score =>
              getTail(bests)
            case _ =>
              new SRCons(head, latestRemainder, tailDepth, getTail(bests + (ready.remainder -> ready.score)))
          }
        case _ =>
          new SRCons(head, latestRemainder, tailDepth, getTail(bests))
      }
    }

    other match {
      case _: SREmpty[Input] => this
      case cons: SRCons[Input, Other] =>
        val latestRemainder = getLatest(this.latestRemainder, other.latestRemainder)
        if (head.score >= cons.head.score) {
          getResult(head, latestRemainder,1 + tailDepth, newBests => tail.merge(cons, mergeDepth + 1, newBests))
        } else
          getResult(cons.head, latestRemainder,1 + cons.tailDepth, newBests => this.merge(cons.tail, mergeDepth + 1, newBests))
      case earlier => earlier.merge(this)
    }
  }

  def getLatest(one: Int, other: Int): Int = {
    if (one > other) one else other
  }

  override def nonEmpty = true

  override def pop(): (LazyParseResult[Input, Result], ParseResults[Input, Result]) = (head, tail)

  override def move(array: ArrayCharSequence, offset: Int) = {
    val newCons = this.updateRemainder(i => i.drop(array, offset)).asInstanceOf[SRCons[Input, Result]]
    new SRCons(newCons.head, latestRemainder + offset, newCons.tailDepth, newCons.tail)
  }
}

object SREmpty {
  private val value = new SREmpty[Nothing]
  def empty[Input <: ParseInput[Input]]: SREmpty[Input] = value.asInstanceOf[SREmpty[Input]]
}

class SREmpty[Input <: ParseInput[Input]] extends ParseResults[Input, Nothing] {
  override def merge[Other >: Nothing](other: ParseResults[Input, Other], depth: Int,
                                       bests: Map[Input, Double] = Map.empty) = other

  override def mapResult[NewResult](f: LazyParseResult[Input, Nothing] => LazyParseResult[Input, NewResult], uniform: Boolean) = this

  override def flatMap[NewResult](f: LazyParseResult[Input, Nothing] => ParseResults[Input, NewResult], uniform: Boolean) = this

  override def map[NewResult](f: Nothing => NewResult) = this

  override def tailDepth = 0

  override def toList = List.empty

  override def nonEmpty = false

  override def pop() = throw new Exception("Can't pop empty results")

  override def latestRemainder = Int.MinValue

  override def move(array: ArrayCharSequence, offset: Int) = this
}