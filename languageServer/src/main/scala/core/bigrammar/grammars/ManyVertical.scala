package core.bigrammar.grammars

import core.bigrammar.BiGrammar

class ManyVertical[Value](inner: BiGrammar[Value], parseGreedy: Boolean = true) extends Many(inner, parseGreedy) {
  override def horizontal = false

  override def withChildren(newChildren: Seq[BiGrammar[_]]) =
    new ManyVertical(newChildren.head.asInstanceOf[BiGrammar[Value]], parseGreedy)

  override def containsParser(recursive: BiGrammar[_] => Boolean): Boolean = recursive(inner)
}
