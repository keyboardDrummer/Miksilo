package core.bigrammar.grammars

import core.bigrammar.BiGrammar
import core.bigrammar.BiGrammarToParser.Result

case class MapGrammarWithMap(var inner: BiGrammar,
                             construct: Result => Result,
                             deconstruct: Result => Option[Result]) extends BiGrammar {

  override def children = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = MapGrammarWithMap(newChildren.head, construct, deconstruct)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)
}
