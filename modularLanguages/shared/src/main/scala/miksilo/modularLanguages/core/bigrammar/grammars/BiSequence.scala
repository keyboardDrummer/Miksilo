package miksilo.modularLanguages.core.bigrammar.grammars

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.bigrammar.printer.UndefinedDestructuringValue

class BiSequence(var first: BiGrammar, var second: BiGrammar,
                 val bijective: SequenceBijective,
                 val horizontal: Boolean) extends BiGrammar with Layout {

  override def withChildren(newChildren: Seq[BiGrammar]) = new BiSequence(newChildren(0), newChildren(1), bijective, horizontal)

  override def children = Seq(first, second)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean =
    recursive(first) || recursive(second)

  override protected def getLeftChildren(recursive: BiGrammar => Seq[BiGrammar]): Seq[BiGrammar] = {
    if (first.containsParser())
      recursive(first)
    else {
      recursive(second)
    }
  }
}

object BiSequence {

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