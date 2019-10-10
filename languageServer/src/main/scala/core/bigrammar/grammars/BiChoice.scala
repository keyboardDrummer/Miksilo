package core.bigrammar.grammars

import core.bigrammar.BiGrammar

class BiChoice[Value](var left: BiGrammar[Value], var right: BiGrammar[Value], val firstIsLonger: Boolean) extends BiGrammar[Value]
{
  override def children = Seq(left, right)

  override def withChildren(newChildren: Seq[BiGrammar[_]]) = new BiChoice[Value](
    newChildren(0).asInstanceOf[BiGrammar[Value]],
    newChildren(1).asInstanceOf[BiGrammar[Value]], firstIsLonger)

  override def containsParser(recursive: BiGrammar[_] => Boolean): Boolean = recursive(left) || recursive(right)
}
