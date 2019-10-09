package core.bigrammar.grammars

import core.bigrammar.BiGrammar

class ManyVertical(inner: BiGrammar, parseGreedy: Boolean = true) extends Many(inner, parseGreedy) {
  override def horizontal = false

  override def withChildren(newChildren: Seq[BiGrammar]) = new ManyVertical(newChildren.head, parseGreedy)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)
}
