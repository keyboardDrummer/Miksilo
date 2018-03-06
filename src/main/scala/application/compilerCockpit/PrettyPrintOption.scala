package application.compilerCockpit

import java.io.InputStream

import core.deltas.Delta

object PrettyPrintOption extends CompileOption {

  override def perform(cockpit: LanguageSandbox, input: InputStream): TextWithGrammar = {
    val prettyPrint = PrettyPrint(recover = true)
    val splicedParticles = Delta.replace(cockpit.deltas, MarkOutputGrammar, Seq(prettyPrint))
    val language = Delta.buildLanguage(splicedParticles)

    val state = language.parseAndTransform(input)
    val outputGrammar = prettyPrint.getOutputGrammar(state.language)
    TextWithGrammar(state.output, outputGrammar)
  }

  override def toString = "Pretty Print"
}
