package core.bigrammar.grammars

import core.bigrammar.{BiGrammar, WithMap}

class MapGrammar(inner: BiGrammar,
                      construct: Any => Any,
                      deconstruct: Any => Option[Any]) extends MapGrammarWithMap(inner,
  withMap => WithMap(construct(withMap.value), withMap.map),
  withMap => deconstruct(withMap.value).map(v => WithMap(v, withMap.map)))
