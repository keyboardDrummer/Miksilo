package application.compilerCockpit

import core.grammar.ToDocument
import core.transformation.TransformationState

object InputGrammarToOutput extends CompileOption {

  override def leave(state: TransformationState): Unit = {
    OutputOption.setOutput(state, ToDocument.toDocument(state.grammarCatalogue).renderString)
  }
}
