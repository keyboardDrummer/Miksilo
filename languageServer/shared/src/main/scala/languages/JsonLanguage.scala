package languages

import core.language.{Language, SourceElement}
import core.parsers.editorParsers.{NeverStop, OffsetPointerRange}
import languages.json.{JsonParser, JsonValue}

trait FakeSourceElement2 extends SourceElement {
  override def range: Option[OffsetPointerRange] = None

  override def uriOption: Option[String] = None
}

object JsonLanguage extends Language {

  object FakeSourceElement2 extends FakeSourceElement2
  private val parsePhase = Language.getCachingParsePhase[JsonValue](
    (program, uri) => FakeSourceElement2,
    JsonParser.valueParser.getWholeInputParser(), NeverStop, indentationSensitive = false) // Change neverstop

  compilerPhases = List(parsePhase)
}

