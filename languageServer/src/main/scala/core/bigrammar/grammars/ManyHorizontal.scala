package core.bigrammar.grammars

import core.bigrammar.BiGrammar

class ManyHorizontal(inner: BiGrammar, parseGreedy: Boolean = true) extends Many(inner, parseGreedy) {
  override def horizontal = true

  override def withChildren(newChildren: Seq[BiGrammar]) = new ManyHorizontal(newChildren.head, parseGreedy)
  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)
}
