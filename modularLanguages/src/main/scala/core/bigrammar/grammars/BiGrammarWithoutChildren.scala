package core.bigrammar.grammars

import core.bigrammar.BiGrammar

trait BiGrammarWithoutChildren extends BiGrammar {
  def children: Seq[BiGrammar] = Seq.empty
  override def withChildren(newChildren: Seq[BiGrammar]) = this
}
