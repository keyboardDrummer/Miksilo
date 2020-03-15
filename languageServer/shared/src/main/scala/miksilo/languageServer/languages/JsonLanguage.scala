package miksilo.languageServer.languages

import miksilo.editorParser.languages.json.{JsonParser, JsonValue}
import miksilo.languageServer.core.language.{Language, SourcePathFromElement}

object JsonLanguage extends Language {

  private val parsePhase = Language.getCachingParsePhase[JsonValue](
    (program, uri) => SourcePathFromElement(uri, program),
    JsonParser.valueParser.getWholeInputParser(), indentationSensitive = false) // Change neverstop

  compilerPhases = List(parsePhase)
}

