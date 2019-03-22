package deltas.yaml

import deltas.json.JsonLanguage

object YamlLanguage {
  val deltas = Seq(YamlLanguageDelta) ++ JsonLanguage.deltas

}
