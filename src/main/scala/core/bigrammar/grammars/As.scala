package core.bigrammar.grammars

import core.bigrammar.BiGrammar
import core.particles.node.NodeField

case class As(var inner: BiGrammar, key: NodeField) extends BiGrammar
{
  override def children: Seq[BiGrammar] = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = As(newChildren.head, key)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)
}
