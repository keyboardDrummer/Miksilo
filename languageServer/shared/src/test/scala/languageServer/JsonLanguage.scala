package languageServer

import core.language.Language
import core.parsers.ParseJson
import core.parsers.editorParsers.{NeverStop, StopImmediately}
import core.smarts.language.modules.FakeSourceElement

object JsonLanguage extends Language {

  object FakeSourceElement2 extends FakeSourceElement
  private val parsePhase = Language.getParsePhaseFromParser[Any](ParseJson)(
    (program, uri) => FakeSourceElement2,
    ParseJson.ParseWholeInput(ParseJson.jsonParser), NeverStop) // Change neverstop

  compilerPhases = List(parsePhase)
}
