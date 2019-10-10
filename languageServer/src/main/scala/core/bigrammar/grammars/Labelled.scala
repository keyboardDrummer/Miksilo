package core.bigrammar.grammars

import core.bigrammar.BiGrammar
import core.language.node.GrammarKey

class Labelled[Value](val name: GrammarKey, var inner: BiGrammar[Value] = BiFailure[Value]()) extends BiGrammar[Value] {

  def addAlternative(addition: BiGrammar[Value], precedence: Boolean = false) {
    if (inner.isInstanceOf[BiFailure[_]])
      inner = addition
    else {
      if (precedence)
        inner = addition | inner
      else
        inner = inner | addition
    }
  }

  override def children = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar[_]]) = new Labelled(name, newChildren.head.asInstanceOf[BiGrammar[Value]])

  override def containsParser(recursive: BiGrammar[_] => Boolean): Boolean = recursive(inner)
}
