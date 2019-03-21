package deltas.cloudformation

import core.deltas._
import core.language.Language
import core.smarts.SolveConstraintsDelta
import deltas.json.JsonLanguage
import deltas.yaml.YamlLanguageDelta

object CloudFormationLanguage {
  val jsonDeltas: Seq[Delta] = Seq(CloudFormationTemplate) ++ JsonLanguage.deltas ++ Seq(SolveConstraintsDelta)
  val jsonLanguage: Language = LanguageFromDeltas(Seq(ParseUsingTextualGrammar) ++ jsonDeltas)

  val yamlDeltas: Seq[Delta] = Seq(RemoveTagsInObjectMemberKeys, TagsToObjectDelta, CloudFormationTemplate) ++ Seq(YamlLanguageDelta) ++ Seq(SolveConstraintsDelta)
  val yamlLanguage: Language = LanguageFromDeltas(Seq(ParseUsingTextualGrammar) ++ yamlDeltas)
}


