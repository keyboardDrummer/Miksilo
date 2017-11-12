package core.bigrammar.grammars

import core.bigrammar.BiGrammar

class ManyVertical(inner: BiGrammar) extends Many(inner) {
  override def horizontal = false

  override def withChildren(newChildren: Seq[BiGrammar]) = new ManyVertical(newChildren.head)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)
}
