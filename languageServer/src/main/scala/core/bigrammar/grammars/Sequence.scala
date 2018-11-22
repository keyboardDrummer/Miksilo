package core.bigrammar.grammars

import core.bigrammar.printer.UndefinedDestructuringValue

object Sequence {

  def identity: SequenceBijective = SequenceBijective(packTuple, unpackTuple)

  private def packTuple: (Any, Any) => (Any, Any) = (a: Any, b: Any) => (a,b)
  private def unpackTuple: Any => Option[(Any, Any)] = {
    case UndefinedDestructuringValue => Some(UndefinedDestructuringValue, UndefinedDestructuringValue)
    case t: (Any, Any) => Some(t)
    case _ => None
  }

  def ignoreLeft = SequenceBijective((a: Any, b: Any) => b, x => Some(UndefinedDestructuringValue, x))
  def ignoreRight = SequenceBijective((a: Any, b: Any) => a, x => Some(x, UndefinedDestructuringValue))
}

case class SequenceBijective(construct: (Any, Any) => Any, destruct: Any => Option[(Any, Any)])