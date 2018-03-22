package deltas.cloudformation

import core.deltas.Delta
import core.language.Language
import core.smarts.SolveConstraintsDelta
import deltas.json.JsonLanguage

object CloudFormationLanguage {
  val deltas: Seq[Delta] = Seq(CloudFormationTemplate) ++ JsonLanguage.deltas ++ Seq(SolveConstraintsDelta)
  val language: Language = Delta.buildLanguage(deltas)
}
