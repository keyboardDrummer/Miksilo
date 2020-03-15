package miksilo.modularLanguages.deltas

import miksilo.modularLanguages.deltas.json.ModularJsonLanguage
import miksilo.modularLanguages.deltas.smithy.SmithyLanguage
import miksilo.modularLanguages.deltas.solidity.SolidityLanguage
import miksilo.modularLanguages.deltas.verilog.VerilogLanguage
import miksilo.modularLanguages.deltas.yaml.ModularYamlLanguage
import miksilo.languageServer.server.LanguageBuilder

object Languages {
  def languages: Seq[LanguageBuilder] = Seq(
    VerilogLanguageBuilder,
    SolidityLanguageBuilder,
    SmithyLanguageBuilder,
    JsonLanguageBuilder,
    YamlLanguageBuilder
  )
}

object VerilogLanguageBuilder extends LanguageBuilder {
  override def key = "verilog"
  override def build(arguments: collection.Seq[String]) = VerilogLanguage.language
}

object SolidityLanguageBuilder extends LanguageBuilder {
  override def key = "solidity"

  override def build(arguments: collection.Seq[String]) = SolidityLanguage.language
}

object SmithyLanguageBuilder extends LanguageBuilder {
  override def key = "smithy"

  override def build(arguments: collection.Seq[String]) = SmithyLanguage.language
}

object JsonLanguageBuilder extends LanguageBuilder {
  override def key = "json"

  override def build(arguments: collection.Seq[String]) = ModularJsonLanguage.language
}

object YamlLanguageBuilder extends LanguageBuilder {
  override def key = "yaml"

  override def build(arguments: collection.Seq[String]) = ModularYamlLanguage.language
}