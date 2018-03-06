package application.compilerCockpit

import java.io.InputStream

import core.deltas._
import core.language.Language
import deltas.javac.JavaLanguage

object CompileAndRunOption extends CompileOption {

  var language: Language = _
  override def initialize(sandbox: LanguageSandbox): Unit = {
    val deltas = Delta.spliceAndFilterTop(sandbox.deltas, JavaLanguage.byteCodeDeltas, Seq(RunWithJVM))
    language = Delta.buildLanguage(deltas)
  }

  override def run(sandbox: LanguageSandbox, inputStream: InputStream): TextWithGrammar = {
    val state = language.parseAndTransform(inputStream)
    TextWithGrammar(state.output)
  }

  override def toString = "Compile and run"
}
