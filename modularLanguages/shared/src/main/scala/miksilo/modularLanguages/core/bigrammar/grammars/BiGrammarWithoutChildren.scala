package miksilo.modularLanguages.core.bigrammar.grammars

import miksilo.modularLanguages.core.bigrammar.BiGrammar

trait BiGrammarWithoutChildren extends BiGrammar {
  def children: Seq[BiGrammar] = Seq.empty
  override def withChildren(newChildren: Seq[BiGrammar]) = this
}
