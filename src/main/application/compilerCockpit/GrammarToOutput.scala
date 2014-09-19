package application.compilerCockpit

import core.grammar.PrintGrammar
import core.transformation.TransformationState

object GrammarToOutput extends CompileOption {


  override def inject(state: TransformationState): Unit = {
    OutputOption.setOutput(state, PrintGrammar.toDocument(state.grammarCatalogue).renderString())
    state.stop = true
    super.inject(state)
  }
}
