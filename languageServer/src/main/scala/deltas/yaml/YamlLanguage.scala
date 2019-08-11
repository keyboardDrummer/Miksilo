package deltas.yaml

import core.deltas.ParseUsingTextualGrammar
import core.parsers.sequences.TimeRatioStopFunction
import deltas.json.JsonLanguage

object YamlLanguage {
  val parserDelta = ParseUsingTextualGrammar(TimeRatioStopFunction(5))
  val deltasWithoutParser = Seq(YamlObjectDelta, YamlArrayDelta, PlainScalarDelta, YamlCoreDelta) ++ JsonLanguage.deltas
  val deltas = Seq(parserDelta) ++ deltasWithoutParser

}
