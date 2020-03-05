package deltas

import deltas.json.JsonLanguage
import deltas.smithy.SmithyLanguage
import deltas.solidity.SolidityLanguage
import deltas.verilog.VerilogLanguage
import deltas.yaml.YamlLanguage
import languageServer.LanguageBuilder

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

  override def build(arguments: collection.Seq[String]) = JsonLanguage.language
}

object YamlLanguageBuilder extends LanguageBuilder {
  override def key = "yaml"

  override def build(arguments: collection.Seq[String]) = YamlLanguage.language
}