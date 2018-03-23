package core.bigrammar.grammars

import core.bigrammar.BiGrammar
import core.bigrammar.BiGrammarToParser.WithMap

case class MapGrammarWithMap(var inner: BiGrammar,
                        construct: WithMap => WithMap,
                        deconstruct: WithMap => Option[WithMap]) extends BiGrammar {

  override def children = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = MapGrammarWithMap(newChildren.head, construct, deconstruct)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)
}
