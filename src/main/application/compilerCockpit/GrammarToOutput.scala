package application.compilerCockpit

import core.grammar.PrintGrammar
import core.transformation.TransformationState

object GrammarToOutput extends CompileOption {

  override def enter(state: TransformationState): Unit = {
    OutputOption.setOutput(state, PrintGrammar.toDocument(state.grammarCatalogue).renderString())
    state.stop = true
  }
}
