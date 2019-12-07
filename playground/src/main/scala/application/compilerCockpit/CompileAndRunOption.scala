package application.compilerCockpit

import java.io.InputStream

import core.deltas._
import core.language.Language
import deltas.RunWithJVM
import deltas.javac.ByteCodeLanguage

object CompileAndRunOption extends CompileOption {

  var language: Language = _
  override def initialize(sandbox: LanguageSandbox): Unit = {
    val deltas = Delta.spliceAndFilterTop(sandbox.deltas, ByteCodeLanguage.byteCodeDeltas, Seq(RunWithJVM))
    language = LanguageFromDeltas(deltas)
  }

  override def run(sandbox: LanguageSandbox, inputStream: InputStream): TextWithGrammar = {
    val state = language.compileString(inputStream)
    TextWithGrammar(state.output)
  }

  override def name = "Compile and run"
}
