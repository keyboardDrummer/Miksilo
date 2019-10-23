package core.parsers.editorParsers

import SortedParseResults._
import core.parsers.core.ParserLike


sealed trait SortedParseResults[Input, +Result] {
  def nonEmpty: Boolean
  def pop(): (LazyParseResult[Input, Result], SortedParseResults[Input, Result])
  def toList: List[LazyParseResult[Input, Result]]
  def tailDepth: Int

  def merge[Other >: Result](other: SortedParseResults[Input, Other], depth: Int = 0,
                             bests: Map[Input, Double] = Map.empty): SortedParseResults[Input, Other]

  def map[NewResult](f: Result => NewResult): SortedParseResults[Input, NewResult] = {
    flatMap(lazyParseResult => singleResult(lazyParseResult.map(f)), uniform = true)
  }

  def mapResult[NewResult](f: LazyParseResult[Input, Result] => LazyParseResult[Input, NewResult], uniform: Boolean): SortedParseResults[Input, NewResult] = {
    flatMap(r => singleResult(f(r)), uniform)
  }

  def flatMap[NewResult](f: LazyParseResult[Input, Result] => SortedParseResults[Input, NewResult], uniform: Boolean): SortedParseResults[Input, NewResult]

  def recursionsFor[SeedResult](parse: ParserLike[SeedResult]): RecursionsList[Input, SeedResult, Result] =
    RecursionsList[Input, SeedResult, Result](List.empty, this)

  def addHistory(errors: History[Input]): SortedParseResults[Input, Result] = {
    mapWithHistory(x => x, errors)
  }

  def mapWithHistory[NewResult](f: ReadyParseResult[Input, Result] => ReadyParseResult[Input, NewResult],
                                oldHistory: History[Input]): SortedParseResults[Input, NewResult] = {
    mapResult(l => l.mapWithHistory(f, oldHistory), uniform = !oldHistory.canMerge)
  }

  def mapReady[NewResult](f: ReadyParseResult[Input, Result] => ReadyParseResult[Input, NewResult],
                          uniform: Boolean): SortedParseResults[Input, NewResult] = {
    mapResult(l => l.mapReady(f, uniform), uniform)
  }

  def flatMapReady[NewResult](f: ReadyParseResult[Input, Result] => SortedParseResults[Input, NewResult], uniform: Boolean): SortedParseResults[Input, NewResult] = {
    flatMap[NewResult](l => l.flatMapReady(f, uniform), uniform)
  }

  def updateRemainder(f: Input => Input) = {
    mapReady(r => ReadyParseResult(r.resultOption, f(r.remainder), r.history), uniform = true)
  }
}

object SortedParseResults {
  def singleResult[Input, Result](parseResult: LazyParseResult[Input, Result]): SortedParseResults[Input, Result] =
    new SRCons(parseResult,0, SREmpty.empty[Input])
}

// TODO: replace List with something that has constant concat operation.
case class RecursiveResults[Input, +Result](recursions: Map[ParserLike[Any], List[RecursiveParseResult[Input, _, Result]]], tail: SortedParseResults[Input, Result])
  extends SortedParseResults[Input, Result] {

  override def nonEmpty = false

  override def pop() = throw new Exception("Can't pop recursions")

  override def toList = tail.toList

  override def tailDepth = 0

  override def merge[Other >: Result](other: SortedParseResults[Input, Other], depth: Int,
                                      bests: Map[Input, Double] = Map.empty): RecursiveResults[Input, Other] = other match {
    case otherRecursions: RecursiveResults[Input, Result] =>
      val merged = this.recursions.foldLeft(otherRecursions.recursions)((acc, entry) => {
        val value = acc.get(entry._1) match {
          case Some(existingValue) => existingValue ++ entry._2
          case None => entry._2
        }
        acc + (entry._1 -> value)
      })
      RecursiveResults(merged, tail.merge(otherRecursions.tail))
    case _ =>
      RecursiveResults(this.recursions, tail.merge(other))
  }

  override def flatMap[NewResult](f: LazyParseResult[Input, Result] => SortedParseResults[Input, NewResult], uniform: Boolean) = {
    RecursiveResults(
      recursions.mapValues(s => s.map(r => r.compose(pr => pr.flatMap(f, uniform)))),
      tail.flatMap(f, uniform))
  }

  override def recursionsFor[SeedResult](parser: ParserLike[SeedResult]) = {
    val remainder = recursions - parser
    RecursionsList(
      recursions.getOrElse(parser, List.empty).asInstanceOf[List[RecursiveParseResult[Input, SeedResult, Result]]],
      if (remainder.isEmpty) tail else RecursiveResults(remainder, tail))
  }

  override def mapWithHistory[NewResult](f: ReadyParseResult[Input, Result] => ReadyParseResult[Input, NewResult],
                                         oldHistory: History[Input]) = {
    if (oldHistory.flawed)
      tail.mapWithHistory(f, oldHistory)
    else
      super.mapWithHistory(f, oldHistory)
  }
}

final class SRCons[Input, +Result](val head: LazyParseResult[Input, Result],
                                   var tailDepth: Int,
                                   _tail: => SortedParseResults[Input, Result]) extends SortedParseResults[Input, Result] {

  // Used for debugging
  def toList: List[LazyParseResult[Input, Result]] = head :: tail.toList

  def getTail = tail
  lazy val tail = _tail

  if (tailDepth == 50) {
    tail
    tailDepth = 0
  }

  def flatMap[NewResult](f: LazyParseResult[Input, Result] => SortedParseResults[Input, NewResult],
                         uniform: Boolean): SortedParseResults[Input, NewResult] = {
    f(head) match {
      case _: SREmpty[Input] => tail.flatMap(f, uniform)
      case cons: SRCons[Input, NewResult] =>

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

  override def map[NewResult](f: Result => NewResult): SRCons[Input, NewResult] = {
    new SRCons(head.map(f), tailDepth + 1, tail.map(f))
  }

  /*
  We don't want multiple results in the list with the same remainder. Instead we can just have the one with the best score.
  To accomplish this, we use the bests parameter.
   */
  override def merge[Other >: Result](other: SortedParseResults[Input, Other],
                                             mergeDepth: Int,
                                             bests: Map[Input, Double] = Map.empty): SortedParseResults[Input, Other] = {
    if (mergeDepth > 200) // Should be 200, since 100 is not enough to let CorrectionJsonTest.realLifeExample2 pass
      return SREmpty.empty[Input]

    def getResult(head: LazyParseResult[Input, Other], tailDepth: Int, getTail: Map[Input, Double] => SortedParseResults[Input, Other]) = {
      head match {
        case ready: ReadyParseResult[Input, Other] =>
          bests.get(ready.remainder) match {
            case Some(previousBest) if previousBest >= ready.score =>
              getTail(bests)
            case _ =>
              new SRCons(head, tailDepth, getTail(bests + (ready.remainder -> ready.score)))
          }
        case _ =>
          new SRCons(head, tailDepth, getTail(bests))
      }
    }

    other match {
      case _: SREmpty[Input] => this
      case cons: SRCons[Input, Other] =>
        if (head.score >= cons.head.score) {
          getResult(head,1 + tailDepth, newBests => tail.merge(cons, mergeDepth + 1, newBests))
        } else
          getResult(cons.head,1 + cons.tailDepth, newBests => this.merge(cons.tail, mergeDepth + 1, newBests))
      case earlier => earlier.merge(this)
    }
  }

  override def nonEmpty = true

  override def pop(): (LazyParseResult[Input, Result], SortedParseResults[Input, Result]) = (head, tail)
}

object SREmpty {
  private val value = new SREmpty[Nothing]
  def empty[Input]: SREmpty[Input] = value.asInstanceOf[SREmpty[Input]]
}

class SREmpty[Input] extends SortedParseResults[Input, Nothing] {
  override def merge[Other >: Nothing](other: SortedParseResults[Input, Other], depth: Int,
                                       bests: Map[Input, Double] = Map.empty) = other

  override def mapResult[NewResult](f: LazyParseResult[Input, Nothing] => LazyParseResult[Input, NewResult], uniform: Boolean) = this

  override def flatMap[NewResult](f: LazyParseResult[Input, Nothing] => SortedParseResults[Input, NewResult], uniform: Boolean) = this

  override def map[NewResult](f: Nothing => NewResult) = this

  override def tailDepth = 0

  override def toList = List.empty

  override def nonEmpty = false

  override def pop() = throw new Exception("Can't pop empty results")
}