package languageServer

import core.language.Language
import core.parsers.editorParsers.{NeverStop, StopImmediately}
import core.smarts.language.modules.FakeSourceElement
import languages.json.JsonParser

object JsonLanguage extends Language {

  object FakeSourceElement2 extends FakeSourceElement
  private val parsePhase = Language.getParsePhaseFromParser[Any](JsonParser)(
    (program, uri) => FakeSourceElement2,
    JsonParser.ParseWholeInput(JsonParser.valueParser), NeverStop) // Change neverstop

  compilerPhases = List(parsePhase)
}
