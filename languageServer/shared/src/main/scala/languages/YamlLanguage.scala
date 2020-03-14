package languages

import core.language.{Language, SourcePathFromElement}
import languages.yaml.{YamlParser, YamlValue}

object YamlLanguage extends Language {

  private val parsePhase = Language.getCachingParsePhase[YamlValue](
    (program, uri) => SourcePathFromElement(uri, program),
    YamlParser.parser, indentationSensitive = true)

  compilerPhases = List(parsePhase)
}
