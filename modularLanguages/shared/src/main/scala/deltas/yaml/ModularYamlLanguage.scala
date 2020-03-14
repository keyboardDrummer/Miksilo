package deltas.yaml

import core.deltas.{LanguageFromDeltas, ParseUsingTextualGrammar}
import miksilo.editorParser.parsers.editorParsers.UntilTimeStopFunction
import deltas.json.ModularJsonLanguage

object ModularYamlLanguage {
  val parserDelta = ParseUsingTextualGrammar(UntilTimeStopFunction(100), indentationSensitive = true)
  val deltasWithoutParser = Seq(YamlObjectDelta, YamlArrayDelta, PlainScalarDelta, YamlCoreDelta) ++ ModularJsonLanguage.deltas
  val deltas = Seq(parserDelta) ++ deltasWithoutParser
  val language = LanguageFromDeltas(deltas)
}
