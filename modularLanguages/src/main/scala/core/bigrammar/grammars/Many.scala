package core.bigrammar.grammars

abstract class Many(var inner: BiGrammar, val parseGreedy: Boolean) extends BiGrammar with Layout
{
  override def children = Seq(inner)
}
