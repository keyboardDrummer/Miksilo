package miksilo.languageServer.languages

import miksilo.editorParser.languages.yaml.{YamlParser, YamlValue}
import miksilo.languageServer.core.language.{Language, SourcePathFromElement}

object YamlLanguage extends Language {

  private val parsePhase = Language.getCachingParsePhase[YamlValue](
    (program, uri) => SourcePathFromElement(uri, program),
    YamlParser.parser, indentationSensitive = true)

  compilerPhases = List(parsePhase)
}
