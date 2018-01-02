package core.bigrammar.grammars

import core.bigrammar.{BiGrammar, WithMapG}

class MapGrammar(inner: BiGrammar,
                      construct: Any => Any,
                      deconstruct: Any => Option[Any]) extends MapGrammarWithMap(inner,
  withMap => WithMapG(construct(withMap.value), withMap.map),
  withMap => deconstruct(withMap.value).map(v => WithMapG(v, withMap.map)))
