package miksilo.modularLanguages.deltas.yaml

import miksilo.modularLanguages.core.deltas.{LanguageFromDeltas, ParseUsingTextualGrammar}
import miksilo.editorParser.parsers.editorParsers.UntilTimeStopFunction
import miksilo.modularLanguages.deltas.json.ModularJsonLanguage

object ModularYamlLanguage {
  val parserDelta = ParseUsingTextualGrammar(UntilTimeStopFunction(100), indentationSensitive = true)
  val deltasWithoutParser = Seq(YamlObjectDelta, YamlArrayDelta, PlainScalarDelta, YamlCoreDelta) ++ ModularJsonLanguage.deltas
  val deltas = Seq(parserDelta) ++ deltasWithoutParser
  val language = LanguageFromDeltas(deltas)
}
