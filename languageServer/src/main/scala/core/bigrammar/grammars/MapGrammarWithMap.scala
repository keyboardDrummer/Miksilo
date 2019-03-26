package core.bigrammar.grammars

import core.bigrammar.BiGrammar
import core.bigrammar.BiGrammarToParser.AnyWithMap

case class MapGrammarWithMap(var inner: BiGrammar,
                             construct: AnyWithMap => AnyWithMap,
                             deconstruct: AnyWithMap => Option[AnyWithMap]) extends BiGrammar {

  override def children = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = MapGrammarWithMap(newChildren.head, construct, deconstruct)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)
}
