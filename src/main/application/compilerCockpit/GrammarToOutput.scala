package application.compilerCockpit

import core.grammar.ToDocument
import core.transformation.TransformationState

object GrammarToOutput extends CompileOption {

  override def enter(state: TransformationState): Unit = {
    OutputOption.setOutput(state, ToDocument.toDocument(state.grammarCatalogue).renderString)
    state.stop = true
  }
}
