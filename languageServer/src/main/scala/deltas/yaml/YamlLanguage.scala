package deltas.yaml

import deltas.expression._
import deltas.json.JsonLanguage

object YamlLanguage {
  val deltas = Seq(DefaultExpressionDelta, YamlObjectDelta, YamlArrayDelta, YamlCoreDelta, PlainScalarDelta) ++ JsonLanguage.deltas
}
