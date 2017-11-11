package core.bigrammar.grammars

import core.bigrammar.{BiGrammar, WithMapG}
import core.bigrammar.BiGrammarToGrammar.WithMap

//TODO deze nog wat meer typed maken met WithState

case class MapGrammarWithMap(var inner: BiGrammar,
                        construct: WithMap => WithMap,
                        deconstruct: WithMap => Option[WithMap])
  extends BiGrammar {

  override def children = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = MapGrammarWithMap(newChildren.head, construct, deconstruct)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)
}

class MapGrammar(inner: BiGrammar,
                      construct: Any => Any,
                      deconstruct: Any => Option[Any],
                      showMap: Boolean = false) extends MapGrammarWithMap(inner,
  withMap => WithMapG(construct(withMap.value), withMap.map),
  withMap => deconstruct(withMap.value).map(v => WithMapG(v, withMap.map)))
