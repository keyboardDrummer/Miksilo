package core.bigrammar.grammars

import core.bigrammar.BiGrammar

class Choice(var left: BiGrammar, var right: BiGrammar, val firstBeforeSecond: Boolean = false) extends BiGrammar
{
  override def children = Seq(left, right)

  override def withChildren(newChildren: Seq[BiGrammar]) = new Choice(newChildren(0), newChildren(1))

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(left) || recursive(right)
}
