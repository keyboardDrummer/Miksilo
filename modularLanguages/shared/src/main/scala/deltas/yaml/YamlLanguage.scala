package deltas.yaml

import core.deltas.{LanguageFromDeltas, ParseUsingTextualGrammar}
import core.parsers.editorParsers.TimeRatioStopFunction
import deltas.json.JsonLanguage

object YamlLanguage {
  val parserDelta = ParseUsingTextualGrammar(TimeRatioStopFunction(5))
  val deltasWithoutParser = Seq(YamlObjectDelta, YamlArrayDelta, PlainScalarDelta, YamlCoreDelta) ++ JsonLanguage.deltas
  val deltas = Seq(parserDelta) ++ deltasWithoutParser
  val language = LanguageFromDeltas(deltas)
}
