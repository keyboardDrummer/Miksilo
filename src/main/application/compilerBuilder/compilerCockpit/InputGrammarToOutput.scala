package application.compilerBuilder.compilerCockpit

import core.grammar.Labelled
import core.responsiveDocument.ResponsiveDocument
import core.transformation.TransformationState
import core.transformation.grammars.ProgramGrammar

object InputGrammarToOutput extends CompileOption {

  override def leave(state: TransformationState): Unit = {
    val program = state.grammarCatalogue.find(ProgramGrammar)
    val reachableGrammars = program.traverse().collect({case x: Labelled => x })
    val document = reachableGrammars.map(grammar => (grammar.toDocument ~ "=>" ~ grammar.inner.toDocument).asInstanceOf[ResponsiveDocument]).reduce((a,b) => a ^^ b)
    OutputOption.setOutput(state, document.renderString)
  }
}
