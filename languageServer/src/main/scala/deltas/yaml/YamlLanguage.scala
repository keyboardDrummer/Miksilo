package deltas.yaml

import deltas.expression._
import deltas.json.JsonLanguage

object YamlLanguage {
  val deltas = Seq(DefaultExpressionDelta("value"), YamlObjectDelta, YamlArrayDelta, PlainScalarDelta, YamlCoreDelta) ++ JsonLanguage.deltas
}
