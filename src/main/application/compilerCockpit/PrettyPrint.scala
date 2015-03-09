package application.compilerCockpit

import core.biGrammar.{BiGrammar, BiGrammarToDocument, PrintFailure}
import core.particles.grammars.ProgramGrammar
import core.particles.{CompilationState, CompilerFromParticles, Particle}
import core.responsiveDocument.ResponsiveDocument

import scala.util.Try

case class PrettyPrint(recover: Boolean = false) extends Particle
{
  override def inject(state: CompilationState): Unit = {
    val foundGrammar = state.grammarCatalogue.find(ProgramGrammar)
    state.data(this) = foundGrammar.deepClone

    state.compilerPhases = List(() => {
      val grammar = state.data(this).asInstanceOf[BiGrammar]
      val documentTry: Try[ResponsiveDocument] = Try(BiGrammarToDocument.toDocument(state.program, grammar))
      val documentTryWithOptionalRecover: Try[ResponsiveDocument] = if (recover) {
        documentTry.recover({ case e: PrintFailure => e.toDocument})
      }
      else {
        documentTry
      }
      val document: ResponsiveDocument = documentTryWithOptionalRecover.get
      state.output = document.renderString()
    })
  }

  override def description: String = "Prints the program by generating a pretty printer from its grammar."
}

object PrettyPrintOption extends CompileOption {

  override def perform(cockpit: CompilerCockpit, input: String): String = {
    val splicedParticles = cockpit.compiler.replace(MarkOutputGrammar,Seq(PrettyPrint(true)))
    val compiler = new CompilerFromParticles(splicedParticles)

    val state = compiler.parseAndTransform(input)

    state.output
  }

  override def toString = "Pretty Print"
}
