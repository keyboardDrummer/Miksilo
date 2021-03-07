package miksilo.modularLanguages.deltas

import miksilo.modularLanguages.deltas.json.ModularJsonLanguage
import miksilo.modularLanguages.deltas.smithy.SmithyLanguage
import miksilo.modularLanguages.deltas.solidity.SolidityLanguage
import miksilo.modularLanguages.deltas.verilog.VerilogLanguage
import miksilo.modularLanguages.deltas.yaml.ModularYamlLanguage
import miksilo.languageServer.server.{LanguageBuilder, SimpleLanguageBuilder}

object Languages {
  def languages: Seq[LanguageBuilder] = Seq(
    VerilogLanguageBuilder,
    SolidityLanguageBuilder,
    SmithyLanguageBuilder,
    JsonLanguageBuilder,
    YamlLanguageBuilder
  )
}

object VerilogLanguageBuilder extends SimpleLanguageBuilder("verilog", VerilogLanguage.language)

object SolidityLanguageBuilder extends SimpleLanguageBuilder("solidity", SolidityLanguage.language)

object SmithyLanguageBuilder extends SimpleLanguageBuilder("smithy", SmithyLanguage.language)

object JsonLanguageBuilder extends SimpleLanguageBuilder("json", ModularJsonLanguage.language)

object YamlLanguageBuilder extends SimpleLanguageBuilder("yaml", ModularYamlLanguage.language)