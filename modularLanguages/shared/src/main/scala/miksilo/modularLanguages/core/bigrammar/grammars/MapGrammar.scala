package miksilo.modularLanguages.core.bigrammar.grammars

import miksilo.modularLanguages.core.bigrammar.BiGrammar

case class MapGrammar[Value, NewValue](var inner: BiGrammar,
                      construct: Value => Either[String, NewValue],
                      deconstruct: NewValue => Option[Value]) extends BiGrammar {

  override def children = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = MapGrammar(newChildren.head, construct, deconstruct)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)
}
