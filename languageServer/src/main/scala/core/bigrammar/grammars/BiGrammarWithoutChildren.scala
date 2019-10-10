package core.bigrammar.grammars

import core.bigrammar.BiGrammar

trait BiGrammarWithoutChildren[Value] extends BiGrammar[Value] {
  def children: Seq[BiGrammar[_]] = Seq.empty
  override def withChildren(newChildren: Seq[BiGrammar[_]]) = this
}
