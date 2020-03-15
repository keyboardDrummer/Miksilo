package miksilo.playground.application.compilerCockpit

import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.core.deltas.{Delta, LanguageFromDeltas}
import miksilo.modularLanguages.deltas.RunWithJVM
import miksilo.modularLanguages.deltas.bytecode.ByteCodeLanguage

object CompileAndRunOption extends CompileOption {

  var language: Language = _
  override def initialize(sandbox: LanguageSandbox): Unit = {
    val deltas = Delta.spliceAndFilterTop(sandbox.deltas, ByteCodeLanguage.byteCodeDeltas, Seq(RunWithJVM))
    language = LanguageFromDeltas(deltas)
  }

  override def run(sandbox: LanguageSandbox, text: String): TextWithGrammar = {
    val state = language.compileString(text)
    TextWithGrammar(state.output)
  }

  override def name = "Compile and run"
}
