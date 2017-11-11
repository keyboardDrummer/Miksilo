package core.bigrammar.grammars

import core.bigrammar.BiGrammar

class ManyHorizontal(inner: BiGrammar) extends Many(inner) {
  override def horizontal = true

  override def withChildren(newChildren: Seq[BiGrammar]) = new ManyHorizontal(newChildren.head)
  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)
}
