package core.bigrammar.grammars

import core.bigrammar.BiGrammar
import core.particles.node.GrammarKey

class Labelled(val name: GrammarKey, var inner: BiGrammar = BiFailure()) extends BiGrammar {

  def addOption(addition: BiGrammar) {
    inner = inner | addition
  }

  override def children = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = new Labelled(name, newChildren.head)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)
}
