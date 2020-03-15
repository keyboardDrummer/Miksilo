package miksilo.modularLanguages.core.bigrammar.grammars

import miksilo.modularLanguages.core.bigrammar.{BiGrammar, WithMap}

class ValueMapGrammar[Value, NewValue](inner: BiGrammar,
                      construct: Value => Either[String, NewValue],
                      deconstruct: NewValue => Option[Value])
  extends MapGrammar[WithMap[Value], WithMap[NewValue]](inner,
  withMap => construct(withMap.value).map(value => WithMap(value, withMap.namedValues)),
  withMap => deconstruct(withMap.value).map(v => WithMap(v, withMap.namedValues)))
