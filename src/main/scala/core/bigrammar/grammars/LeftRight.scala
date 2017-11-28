package core.bigrammar.grammars

import core.bigrammar.BiGrammar

class LeftRight(var first: BiGrammar, var second: BiGrammar) extends BiGrammar with Sequence
{
  override def horizontal = true

  override def withChildren(newChildren: Seq[BiGrammar]) = new LeftRight(newChildren(0), newChildren(1))
}
