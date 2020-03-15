package languages

import miksilo.languageServer.core.language.{Language, SourcePathFromElement}
import miksilo.languageServer.core.parsers.SourceElement
import miksilo.editorParser.parsers.editorParsers.{NeverStop, OffsetPointerRange}
import languages.json.{JsonParser, JsonValue}

object JsonLanguage extends Language {

  private val parsePhase = Language.getCachingParsePhase[JsonValue](
    (program, uri) => SourcePathFromElement(uri, program),
    JsonParser.valueParser.getWholeInputParser(), indentationSensitive = false) // Change neverstop

  compilerPhases = List(parsePhase)
}

