package core.bigrammar.grammars

import core.bigrammar.BiGrammar

class ManyHorizontal[Value](inner: BiGrammar[Value], parseGreedy: Boolean = true) extends Many(inner, parseGreedy) {
  override def horizontal = true

  override def withChildren(newChildren: Seq[BiGrammar[_]]) =
    new ManyHorizontal(newChildren.head.asInstanceOf[BiGrammar[Value]], parseGreedy)
  override def containsParser(recursive: BiGrammar[_] => Boolean): Boolean = recursive(inner)
}
