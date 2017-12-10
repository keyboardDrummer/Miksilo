package application.compilerCockpit

import java.io.InputStream

import core.bigrammar.BiGrammarToGrammar
import core.deltas.Language

object PrettyPrintOption extends CompileOption {

  override def perform(cockpit: CompilerCockpit, input: InputStream): TextWithGrammar = {
    val prettyPrint = PrettyPrint(recover = true)
    val splicedParticles = Language.replace(cockpit.language.deltas, MarkOutputGrammar,Seq(prettyPrint))
    val language = new Language(splicedParticles)

    val state = language.parseAndTransform(input)
    val outputGrammar = prettyPrint.getOutputGrammar(state.language)
    TextWithGrammar(state.output, BiGrammarToGrammar.toGrammar(outputGrammar))
  }

  override def toString = "Pretty Print"
}
