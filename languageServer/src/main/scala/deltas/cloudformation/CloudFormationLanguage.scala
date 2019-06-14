package deltas.cloudformation

import core.deltas._
import core.language.Language
import core.smarts.SolveConstraintsDelta
import deltas.json.JsonLanguage
import deltas.yaml.YamlLanguage

object CloudFormationLanguage {
  val jsonDeltas: Seq[Delta] = Seq(CloudFormationTemplate) ++
    JsonLanguage.deltas ++ Seq(SolveConstraintsDelta)
  val jsonLanguage: Language = LanguageFromDeltas(Seq(ParseUsingTextualGrammar) ++ jsonDeltas)

  val yamlDeltas: Seq[Delta] = Seq(ConvertObjectMemberKeysToStrings, ConvertTagsToObjectDelta, CloudFormationTemplate) ++
    YamlLanguage.deltas ++ Seq(SolveConstraintsDelta)
  val yamlLanguage: Language = LanguageFromDeltas(Seq(ParseUsingTextualGrammar) ++ yamlDeltas)
}


