package core.bigrammar.grammars

import core.bigrammar.BiGrammar

abstract class Many(var inner: BiGrammar, val parseGreedy: Boolean) extends BiGrammar with Layout
{
  override def children = Seq(inner)
}
