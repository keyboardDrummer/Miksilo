package core.bigrammar.grammars

import core.bigrammar.BiGrammar

class Sequence(var first: BiGrammar, var second: BiGrammar) extends BiGrammar with SequenceLike
{
  override def horizontal = true

  override def withChildren(newChildren: Seq[BiGrammar]) = new Sequence(newChildren(0), newChildren(1))
}
