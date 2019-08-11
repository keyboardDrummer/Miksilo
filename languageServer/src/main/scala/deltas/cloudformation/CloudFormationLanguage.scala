package deltas.cloudformation

import core.deltas._
import core.language.Language
import core.parsers.editorParsers.TimeRatioStopFunction
import core.smarts.SolveConstraintsDelta
import deltas.json.JsonLanguage
import deltas.yaml.YamlLanguage

object CloudFormationLanguage {
  val jsonDeltas: Seq[Delta] = Seq(CloudFormationTemplate) ++
    JsonLanguage.deltas ++ Seq(SolveConstraintsDelta)
  val jsonLanguage: Language = LanguageFromDeltas(Seq(ParseUsingTextualGrammar(TimeRatioStopFunction(10))) ++ jsonDeltas)

  val yamlDeltas: Seq[Delta] = Seq(YamlLanguage.parserDelta) ++
    Seq(ConvertObjectMemberKeysToStrings, ConvertTagsToObjectDelta, CloudFormationTemplate) ++
    YamlLanguage.deltasWithoutParser ++ Seq(SolveConstraintsDelta)

  val yamlLanguage: Language = LanguageFromDeltas(yamlDeltas)
}


