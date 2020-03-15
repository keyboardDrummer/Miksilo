package miksilo.modularLanguages.core.bigrammar.grammars

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.node.GrammarKey

class Labelled(val name: GrammarKey, var inner: BiGrammar = BiFailure()) extends BiGrammar {

  def addAlternative(addition: BiGrammar, precedence: Boolean = false): Unit = {
    if (inner.isInstanceOf[BiFailure])
      inner = addition
    else {
      if (precedence)
        inner = addition | inner
      else
        inner = inner | addition
    }
  }

  override def children = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = new Labelled(name, newChildren.head)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)
}
