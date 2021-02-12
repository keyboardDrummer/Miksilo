package miksilo.editorParser.parsers.editorParsers

import miksilo.editorParser.parsers.core.OffsetPointer

trait PairingResults[State, +Result] extends DelayedResults[State, Result] {

//  override def mapResult[NewResult](f: LazyParseResult[State, Result] => LazyParseResult[State, NewResult],
//                                    uniform: Boolean): ParseResults[State, NewResult] = {
//    new MapDelayed(this, f)
//  }

  override def flatMap[NewResult](f: LazyParseResult[State, Result] => ParseResults[State, NewResult],
                                  uniform: Boolean): ParseResults[State, NewResult] = {
    new FlatMapPairing(this, f)
  }

  override def toList: List[LazyParseResult[State, Result]] = ???

  override def latestRemainder: OffsetPointer = ???

}

class PairingNode[State, Result](val value: DelayedParseResult[State, Result],
                          val children: List[DelayedResults[State, Result]] = List.empty)
  extends PairingResults[State, Result] {

  override def pop(): Option[(DelayedParseResult[State, Result], DelayedResults[State, Result])] = {
    val tail: DelayedResults[State, Result] =
      if (children.isEmpty) new NilDelayed[State]()
      else children.reduce[DelayedResults[State, Result]]((a,b) => a.merge(b).asInstanceOf[DelayedResults[State, Result]])
    Some(value, tail)
  }

  override def merge[Other >: Result](other: ParseResults[State, Other]): ParseResults[State, Other] = {
    other match {
      case otherNode: PairingNode[State, Result] =>
        if (value.score > otherNode.value.score)
          new PairingNode(value, otherNode :: children)
        else {
          new PairingNode(otherNode.value, this :: otherNode.children)
        }
      case _ => other.merge(this)
    }
  }
}

//class MapDelayed[State, Result, +NewResult](original: PairingDelayedResults[State, Result],
//                                            f: LazyParseResult[State, Result] => LazyParseResult[State, NewResult])
//  extends PairingDelayedResults[State, NewResult] {
//
//  override def pop(): Option[(DelayedParseResult[State, NewResult], DelayedResults[State, NewResult])] = {
//    original.pop().map(p => (
//      f(p._1).asInstanceOf[DelayedParseResult[State, NewResult]],
//      p._2.mapResult(f, uniform = true).asInstanceOf[DelayedResults[State, NewResult]]))
//  }
//}
//

class FlatMapPairing[State, Result, +NewResult](original: DelayedResults[State, Result],
                                                f: LazyParseResult[State, Result] => ParseResults[State, NewResult])
  extends PairingResults[State, NewResult] {

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

  override def merge[Other >: NewResult](other: ParseResults[State, Other]): ParseResults[State, Other] = {
    other match {
      case ready: ReadyResults[State, Other] => ready.merge(this)
      case _ =>
        pop().fold(other)(popped => {
          new PairingNode(popped._1, List(popped._2)).merge(other)
        })
    }
  }
}

//class PairingNilDelayed[State] extends PairingDelayedResults[State, Nothing] {
//  override def pop(): Option[(LazyParseResult[State, Nothing], ParseResults[State, Nothing])] = None
//
//  override def merge[Other >: Nothing](other: ParseResults[State, Other]): ParseResults[State, Other] = other
//
//  override def flatMap[NewResult](f: LazyParseResult[State, Nothing] => ParseResults[State, NewResult],
//                                  uniform: Boolean): ParseResults[State, NewResult] = this
//
//  override def latestRemainder: OffsetPointer = ???
//
//  override def toList: List[LazyParseResult[State, Nothing]] = ???
//}