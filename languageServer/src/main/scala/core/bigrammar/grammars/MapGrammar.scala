package core.bigrammar.grammars

import core.bigrammar.BiGrammar

case class MapGrammar[Value, NewValue](var inner: BiGrammar[Value],
                      construct: Value => Either[String, NewValue],
                      deconstruct: NewValue => Option[Value]) extends BiGrammar[NewValue] {

  override def children = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar[_]]) =
    MapGrammar(newChildren.head.asInstanceOf[BiGrammar[Value]], construct, deconstruct)

  override def containsParser(recursive: BiGrammar[_] => Boolean): Boolean = recursive(inner)
}
