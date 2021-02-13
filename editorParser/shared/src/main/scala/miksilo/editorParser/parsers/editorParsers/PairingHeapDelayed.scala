package miksilo.editorParser.parsers.editorParsers

import miksilo.editorParser.parsers.core.OffsetPointer

trait PairingResults[State, +Result] extends DelayedResults[State, Result] {

//  override def mapResult[NewResult](f: LazyParseResult[State, Result] => LazyParseResult[State, NewResult]): ParseResults[State, NewResult] = {
//    new MapPairing(this, f)
//  }

  override def flatMap[NewResult](f: LazyParseResult[State, Result] => ParseResults[State, NewResult]): ParseResults[State, NewResult] = {
    ??? //new FlatMapPairing(this, f)
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

}

case class PairingNode[State, Result](value: DelayedParseResult[State, Result],
                                 children: List[DelayedResults[State, Result]] = List.empty)
  extends PairingResults[State, Result] {

//  override def mapResult[NewResult](f: LazyParseResult[State, Result] => LazyParseResult[State, NewResult]): ParseResults[State, NewResult] = {
//    new PairingNode(f(value).asInstanceOf[DelayedParseResult[State, NewResult]],
//      children.map(c => c.mapResult(f).asInstanceOf[DelayedResults[State, NewResult]]))
//  }

  override def flatMap[NewResult](f: LazyParseResult[State, Result] => ParseResults[State, NewResult]): ParseResults[State, NewResult] = {
    // I don't trust the f(value) call, seems to be the same as what sortedDelayedList does though
    val result = children.map(c => c.flatMap(f)).foldLeft(f(value))((a,b) => a.merge(b))
    result
  }

  override def pop(): Option[(DelayedParseResult[State, Result], DelayedResults[State, Result])] = {
    val tail: DelayedResults[State, Result] =
      if (children.isEmpty) new NilDelayed[State]()
      else children.reduce[DelayedResults[State, Result]]((a,b) => a.merge(b).asInstanceOf[DelayedResults[State, Result]])
    Some(value, tail)
  }

  override def merge[Other >: Result](other: ParseResults[State, Other]): ParseResults[State, Other] = {
    other match {
      case otherNode: PairingNode[State, Result] =>
        if (value.score >= otherNode.value.score)
          PairingNode(value, otherNode :: children)
        else {
          PairingNode(otherNode.value, this :: otherNode.children)
        }
      case _ => other.merge(this)
    }
  }
//  override def merge[Other >: Result](other: ParseResults[State, Other]): ParseResults[State, Other] = {
//    other match {
//      case otherNode: PairingNode[State, Result] =>
//        if (value.score >= otherNode.value.score)
//          PairingNode(value, otherNode :: children)
//        else {
//          PairingNode(otherNode.value, this :: otherNode.children)
//        }
//      case _ => other.merge(this)
//    }
//  }

  override def latestRemainder: OffsetPointer = (value.initialOffset :: children.map(c => c.latestRemainder)).max
}

trait LazyPairingResults[State, +Result] extends PairingResults[State, Result] {
  def original: DelayedResults[State, Any]

  override def latestRemainder: OffsetPointer = original.latestRemainder

  override def merge[Other >: Result](other: ParseResults[State, Other]): ParseResults[State, Other] = {
    other match {
      case ready: ReadyResults[State, Other] => ready.merge(this)
      case _ =>
        pop().fold(other)(popped => {
          PairingNode(popped._1, List(popped._2)).merge(other)
        })
    }
  }
}

class MapPairing[State, Result, +NewResult](val original: DelayedResults[State, Result],
                                            f: LazyParseResult[State, Result] => LazyParseResult[State, NewResult])
  extends LazyPairingResults[State, NewResult] {

  override def pop(): Option[(DelayedParseResult[State, NewResult], DelayedResults[State, NewResult])] = {
    original.pop().map(p => (
      f(p._1).asInstanceOf[DelayedParseResult[State, NewResult]],
      p._2.mapResult(f).asInstanceOf[DelayedResults[State, NewResult]]))
  }
}

class FlatMapPairing[State, Result, +NewResult](val original: DelayedResults[State, Result],
                                                f: LazyParseResult[State, Result] => ParseResults[State, NewResult])
  extends LazyPairingResults[State, NewResult] {

  override def pop(): Option[(DelayedParseResult[State, NewResult], DelayedResults[State, NewResult])] = {
    original.pop().flatMap(originalPop => {
      val applied = f(originalPop._1).asInstanceOf[DelayedResults[State, NewResult]]
      val appliedPop = applied.pop()
      val result = appliedPop.map(p => (
        p._1.asInstanceOf[DelayedParseResult[State, NewResult]],
        p._2.merge(new FlatMapPairing(originalPop._2, f)).asInstanceOf[DelayedResults[State, NewResult]]))
      result
    })
  }
}