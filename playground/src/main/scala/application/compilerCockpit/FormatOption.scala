package application.compilerCockpit

import java.io.InputStream

import core.deltas.LanguageFromDeltas
import core.language.Language
import deltas.PrettyPrint

object FormatOption extends CompileOption {

  val prettyPrint = PrettyPrint(recover = true)
  var language: Language = _

  override def initialize(sandbox: LanguageSandbox): Unit = {
    val startWithPrettyPrint = Seq(PrettyPrint(recover = true)) ++ sandbox.deltas
    language = LanguageFromDeltas(startWithPrettyPrint)
  }

  override def run(sandbox: LanguageSandbox, input: InputStream): TextWithGrammar = {
    val state = language.compileStream(input)
    val outputGrammar = prettyPrint.getOutputGrammar(state.language)
    TextWithGrammar(state.output, outputGrammar)
  }

  override def name = "Reformat code"
}
