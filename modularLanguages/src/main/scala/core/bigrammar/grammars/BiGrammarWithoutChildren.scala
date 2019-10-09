package core.bigrammar.grammars

trait BiGrammarWithoutChildren extends BiGrammar {
  def children: Seq[BiGrammar] = Seq.empty
  override def withChildren(newChildren: Seq[BiGrammar]) = this
}
