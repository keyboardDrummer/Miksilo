package core.bigrammar.grammars

import core.bigrammar.BiGrammar

class TopBottom(var first: BiGrammar, var second: BiGrammar,
                combineValue: (Any, Any) => Any = (a,b) => (a,b))
  extends BiGrammar with Sequence {

  override lazy val height: Int = first.height + second.height

  override def horizontal = false

  override def withChildren(newChildren: Seq[BiGrammar]) = new TopBottom(newChildren(0), newChildren(1), combine)

  override def combine(firstValue: Any, secondValue: Any): Any = this.combineValue(firstValue, secondValue)
}
