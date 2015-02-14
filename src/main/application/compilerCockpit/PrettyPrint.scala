package application.compilerCockpit

import core.grammarDocument.{BiGrammar, BiGrammarToDocument, PrintFailure}
import core.responsiveDocument.ResponsiveDocument
import core.transformation.{TransformationState, CompilerFromParticles}
import core.transformation.grammars.ProgramGrammar
import core.transformation.sillyCodePieces.Particle

import scala.util.Try

object PrettyPrint extends Particle
{
  override def inject(state: TransformationState): Unit = {
    val foundGrammar = state.grammarCatalogue.find(ProgramGrammar)
    state.data(this) = foundGrammar.deepClone

    state.compilerPhases = List(() => {
      val grammar = state.data(this).asInstanceOf[BiGrammar]
      val document: ResponsiveDocument = Try(BiGrammarToDocument.toDocument(state.program, grammar)).
        recover({ case e: PrintFailure => e.toDocument }).get
      state.output = document.renderString()
    })
  }
}

object PrettyPrintOption extends CompileOption {

  override def perform(cockpit: CompilerCockpit, input: String): String = {
    val splicedParticles = cockpit.compiler.replace(CockpitOutputMarker,Seq(PrettyPrint))
    val compiler = new CompilerFromParticles(splicedParticles)

    val state = compiler.parseAndTransform(input)

    state.output
  }

  override def toString = "Pretty Print"
}
