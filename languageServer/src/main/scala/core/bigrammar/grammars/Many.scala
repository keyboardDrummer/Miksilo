package core.bigrammar.grammars

import core.bigrammar.{BiGrammar, WithMap}

abstract class Many[Value](var inner: BiGrammar[Value], val parseGreedy: Boolean)
  extends BiGrammar[List[Value]] with Layout
{
  override def children = Seq(inner)
}

class ManyUnit(var inner: BiGrammar[WithMap[Unit]], val parseGreedy: Boolean, val horizontal: Boolean)
  extends BiGrammar[WithMap[Unit]] with Layout
{
  override def children = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar[_]]) =
    new ManyUnit(newChildren.head.asInstanceOf[BiGrammar[WithMap[Unit]]], parseGreedy, horizontal)

  override def containsParser(recursive: BiGrammar[_] => Boolean) = recursive(inner)
}