package languageServer

import core.language.Language
import core.parsers.editorParsers.NeverStop
import core.smarts.language.modules.FakeSourceElement
import languages.json.JsonParser

object JsonLanguage extends Language {

  object FakeSourceElement2 extends FakeSourceElement
  private val parsePhase = Language.getParsePhaseFromParser[Any](
    (program, uri) => FakeSourceElement2,
    JsonParser.valueParser.getWholeInputParser(), NeverStop) // Change neverstop

  compilerPhases = List(parsePhase)
}
