package miksilo.modularLanguages.core.bigrammar.grammars

import miksilo.modularLanguages.core.bigrammar.BiGrammar

abstract class Many(var inner: BiGrammar, val parseGreedy: Boolean) extends BiGrammar with Layout
{
  override def children = Seq(inner)
}
