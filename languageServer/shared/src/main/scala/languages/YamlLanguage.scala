package languages

import core.language.Language
import languages.yaml.{YamlParser, YamlValue}

object YamlLanguage extends Language {

  object FakeSourceElement2 extends FakeSourceElement2
  private val parsePhase = Language.getCachingParsePhase[YamlValue](
    (program, uri) => FakeSourceElement2,
    YamlParser.parser, indentationSensitive = true)

  compilerPhases = List(parsePhase)
}
