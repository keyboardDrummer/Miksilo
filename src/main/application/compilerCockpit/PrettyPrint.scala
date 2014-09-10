package application.compilerCockpit

import core.grammarDocument.{GrammarDocument, PrintValueUsingGrammarDocument}
import core.transformation.TransformationState
import core.transformation.grammars.ProgramGrammar

object PrettyPrint extends CompileOption {

  override def enter(state: TransformationState): Unit = {
    val programGrammar = state.grammarCatalogue.find(ProgramGrammar)
    state.data(this) = programGrammar.deepClone
  }

  override def leave(state: TransformationState): Unit = {
    val programGrammar = state.data(this).asInstanceOf[GrammarDocument]
    val result = PrintValueUsingGrammarDocument.toDocument(state.program, programGrammar).renderString()
    OutputOption.setOutput(state, result)
  }

  override def toString = "Pretty Print"
}
