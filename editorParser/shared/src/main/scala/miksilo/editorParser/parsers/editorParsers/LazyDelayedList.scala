package miksilo.editorParser.parsers.editorParsers

import miksilo.editorParser.parsers.core.OffsetPointer

trait LazyDelayedResults[State, +Result] extends DelayedResults[State, Result] {
  override def mapWithHistory[NewResult](f: ReadyParseResult[State, Result] => ReadyParseResult[State, NewResult],
                                         oldHistory: History): ParseResults[State, NewResult] = {
    mapResult(l => l.mapWithHistory(f, oldHistory), uniform = !oldHistory.canMerge)
  }

  override def flatMap[NewResult](f: LazyParseResult[State, Result] => ParseResults[State, NewResult],
                                  uniform: Boolean): ParseResults[State, NewResult] = {
    new FlatMapDelayed(this, f)
  }


  override def mapResult[NewResult](f: LazyParseResult[State, Result] => LazyParseResult[State, NewResult],
                                    uniform: Boolean): ParseResults[State, NewResult] = {
    new MapPairing(this, f)
  }

  override def merge[Other >: Result](other: ParseResults[State, Other]): ParseResults[State, Other] = {
    other match {
      case _: NilDelayed[State] => this
//      case mergeDelayedList: MergeDelayedList[State, Result] => mergeDelayedList.merge(this)
//      case delayedOther: LazyDelayedResults[State, Result] => new MergeDelayedList(List(this, delayedOther))
      case delayedOther: LazyDelayedResults[State, Result] => new MergeDelayed(this, delayedOther)
      case _ => other.merge(this)
    }

  }

  override def toList: List[LazyParseResult[State, Result]] = {
    var reverseResult = List.empty[LazyParseResult[State, Result]]
    var remainder: ParseResults[State, Result] = this
    var pop = remainder.pop()
    while(pop.nonEmpty) {
      reverseResult ::= pop.get._1
      remainder = pop.get._2
      pop = remainder.pop()
    }
    reverseResult.reverse
  }

  override def latestRemainder: OffsetPointer = ???
}

class MergeDelayedList[State, +Result](val options: List[DelayedResults[State, Result]])
  extends LazyDelayedResults[State, Result] {

  override def merge[Other >: Result](other: ParseResults[State, Other]): ParseResults[State, Other] = {
    other match {
      case mergeDelayedList: MergeDelayedList[State, Result] => new MergeDelayedList(options ++ mergeDelayedList.options)
      case delayedOther: DelayedResults[State, Result] => new MergeDelayedList(delayedOther :: options)
      case _ => other.merge(this)
    }

  }

  override def pop(): Option[(DelayedParseResult[State, Result], DelayedResults[State, Result])] = {
    val pops = options.flatMap(o => o.pop().toSeq)
    if (pops.isEmpty) {
      return None
    }

    if (pops.tail.isEmpty) {
      return Some(pops.head)
    }

    val heads = pops.map(p => p._1).sortBy(d => d.score)(Ordering.Double.IeeeOrdering.reverse)
    val tails = pops.map(p => p._2).reduce[DelayedResults[State, Result]]((a, b) => a.merge(b).asInstanceOf[DelayedResults[State, Result]]).
      asInstanceOf[LazyDelayedResults[State, Result]]

    val poppedChain = heads.tail.foldLeft[DelayedResults[State, Result]](new NilDelayed[State])((a,b) => new ConsDelayed(b, a))
    Some(heads.head, poppedChain.merge(tails).asInstanceOf[DelayedResults[State, Result]])
  }
}

class MergeDelayed[State, +Result](first: LazyDelayedResults[State, Result], second: LazyDelayedResults[State, Result])
  extends LazyDelayedResults[State, Result] {

  override def pop(): Option[(DelayedParseResult[State, Result], DelayedResults[State, Result])] = {
    val firstPop = first.pop()
    val secondPop = second.pop()
    (firstPop, secondPop) match {
      case (None, _) => secondPop
      case (_, None) => firstPop
      case (Some((firstHead, firstTail)), Some((secondHead, secondTail))) =>
        if (firstHead.score > secondHead.score) {
          val newSecondTail = second match {
            case cons: ConsDelayed[State ,Result] => cons
            case _ => new ConsDelayed[State, Result](secondHead).merge(secondTail)
          }
          Some(firstHead, firstTail.merge(newSecondTail).asInstanceOf[DelayedResults[State, Result]])
        } else {
          val newFirstTail = first match {
            case cons: ConsDelayed[State ,Result] => cons
            case _ => new ConsDelayed[State, Result](firstHead).merge(firstTail)
          }
          Some(secondHead, secondTail.merge(newFirstTail).asInstanceOf[DelayedResults[State, Result]])
        }
    }
  }
}

class MapDelayed[State, Result, +NewResult](original: DelayedResults[State, Result],
                                            f: LazyParseResult[State, Result] => LazyParseResult[State, NewResult])
  extends LazyDelayedResults[State, NewResult] {

  override def pop(): Option[(DelayedParseResult[State, NewResult], DelayedResults[State, NewResult])] = {
    original.pop().map(p => (
      f(p._1).asInstanceOf[DelayedParseResult[State, NewResult]],
      p._2.mapResult(f, uniform = true).asInstanceOf[DelayedResults[State, NewResult]]))
  }
}

class FlatMapDelayed[State, Result, +NewResult](original: DelayedResults[State, Result],
                                                f: LazyParseResult[State, Result] => ParseResults[State, NewResult])
  extends LazyDelayedResults[State, NewResult] {

  override def pop(): Option[(DelayedParseResult[State, NewResult], DelayedResults[State, NewResult])] = {
    original.pop().flatMap(originalPop => {
      val applied = f(originalPop._1).asInstanceOf[DelayedResults[State, NewResult]]
      val appliedPop = applied.pop()
      val result = appliedPop.map(p => (
        p._1.asInstanceOf[DelayedParseResult[State, NewResult]],
        p._2.merge(originalPop._2.flatMap(f, uniform = true)).asInstanceOf[DelayedResults[State, NewResult]]))
      result
    })
  }
}

class ConsDelayed[State, +Result](head: DelayedParseResult[State, Result], tail: DelayedResults[State, Result] = new NilDelayed[State])
  extends LazyDelayedResults[State, Result] {

  override def pop(): Option[(DelayedParseResult[State, Result], DelayedResults[State, Result])] = {
    Some(head, tail)
  }
}

