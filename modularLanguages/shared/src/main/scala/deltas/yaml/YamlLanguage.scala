package deltas.yaml

import core.deltas.{LanguageFromDeltas, ParseUsingTextualGrammar}
import core.parsers.editorParsers.UntilTimeStopFunction
import deltas.json.JsonLanguage

object YamlLanguage {
  val parserDelta = ParseUsingTextualGrammar(UntilTimeStopFunction(100), useCaching = false)
  val deltasWithoutParser = Seq(YamlObjectDelta, YamlArrayDelta, PlainScalarDelta, YamlCoreDelta) ++ JsonLanguage.deltas
  val deltas = Seq(parserDelta) ++ deltasWithoutParser
  val language = LanguageFromDeltas(deltas)
}
