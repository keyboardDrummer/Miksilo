package core.bigrammar.grammars

import core.bigrammar.BiGrammar

//TODO deze nog wat meer typed maken met WithState
case class MapGrammar(var inner: BiGrammar,
                      construct: Any => Any,
                      deconstruct: Any => Option[Any],
                      showMap: Boolean = false) extends BiGrammar
{
  override def children = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = new MapGrammar(newChildren.head, construct, deconstruct, showMap)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)
}
