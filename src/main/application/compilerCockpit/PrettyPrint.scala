package application.compilerCockpit

import core.grammarDocument.{BiGrammar, PrintFailure, BiGrammarToDocument}
import core.responsiveDocument.ResponsiveDocument
import core.transformation.TransformationState
import core.transformation.grammars.ProgramGrammar

import scala.util.Try

object PrettyPrint extends CompileOption {

  override def inject(state: TransformationState): Unit = {
    val programGrammar = state.grammarCatalogue.find(ProgramGrammar)
    state.data(this) = programGrammar.deepClone
  }

  override def leave(state: TransformationState): Unit = {
    val programGrammar = state.data(this).asInstanceOf[BiGrammar]
    val document: ResponsiveDocument = Try(BiGrammarToDocument.toDocument(state.program, programGrammar)).
      recover({ case e: PrintFailure => e.toDocument }).get
    val result = document.renderString()
    OutputOption.setOutput(state, result)
    state.stop = true
  }

  override def toString = "Pretty Print"
}
