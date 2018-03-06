package application.compilerCockpit

import java.io.InputStream

import core.deltas.Delta
import core.language.Language

object FormatOption extends CompileOption {

  val prettyPrint = PrettyPrint(recover = true)
  var language: Language = _

  override def initialize(sandbox: LanguageSandbox): Unit = {
    val startWithPrettyPrint = Seq(PrettyPrint(recover = true)) ++ sandbox.deltas
    language = Delta.buildLanguage(startWithPrettyPrint)
  }

  override def run(sandbox: LanguageSandbox, input: InputStream): TextWithGrammar = {
    val state = language.parseAndTransform(input)
    val outputGrammar = prettyPrint.getOutputGrammar(state.language)
    TextWithGrammar(state.output, outputGrammar)
  }

  override def toString = "Reformat code"
}
