package deltas.cloudformation

import java.io.InputStream

import core.SolveConstraintsDelta
import core.deltas._
import core.language.Language
import core.parsers.editorParsers.TimeRatioStopFunction
import deltas.json.JsonLanguage
import deltas.yaml.YamlLanguage

class CloudFormationLanguage(resourceSpecificationOption: Option[InputStream]) {
  val cloudFormationTemplate = new CloudFormationTemplate(resourceSpecificationOption)
  val jsonDeltas: Seq[Delta] = Seq(cloudFormationTemplate) ++
    JsonLanguage.deltas ++ Seq(SolveConstraintsDelta)
  val jsonLanguage: Language = LanguageFromDeltas(Seq(ParseUsingTextualGrammar(TimeRatioStopFunction(10))) ++ jsonDeltas)

  val yamlDeltas: Seq[Delta] = Seq(YamlLanguage.parserDelta) ++
    Seq(ConvertObjectMemberKeysToStrings, ConvertTagsToObjectDelta, cloudFormationTemplate) ++
    YamlLanguage.deltasWithoutParser ++ Seq(SolveConstraintsDelta)

  val yamlLanguage: Language = LanguageFromDeltas(yamlDeltas)
}


