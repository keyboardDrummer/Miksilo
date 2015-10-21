package application.compilerCockpit

import java.io.InputStream

import core.bigrammar.printer.{BiGrammarToPrinter, PrintError, BiGrammarToPrinter$}
import core.bigrammar.BiGrammar
import core.particles.grammars.ProgramGrammar
import core.particles.{Phase, CompilationState, CompilerFromParticles, Particle}
import core.responsiveDocument.ResponsiveDocument

import scala.util.Try

case class PrettyPrint(recover: Boolean = false) extends Particle
{
  override def inject(state: CompilationState): Unit = {
    val foundGrammar = state.grammarCatalogue.find(ProgramGrammar)
    state.data(this) = foundGrammar.deepClone

    state.compilerPhases = List(new Phase(this.name, this.description, () => {
      val grammar = state.data(this).asInstanceOf[BiGrammar]
      val documentTry: Try[ResponsiveDocument] = Try(BiGrammarToPrinter.toDocument(state.program, grammar))
      val documentTryWithOptionalRecover: Try[ResponsiveDocument] = if (recover) {
        documentTry.recover({ case e: PrintError => e.toDocument})
      }
      else {
        documentTry
      }
      val document: ResponsiveDocument = documentTryWithOptionalRecover.get
      state.output = document.renderString()
    }))
  }

  override def description: String = "Prints the program by generating a pretty printer from its grammar."
}

object PrettyPrintOption extends CompileOption {

  override def perform(cockpit: CompilerCockpit, input: InputStream): String = {
    val splicedParticles = cockpit.compiler.replace(MarkOutputGrammar,Seq(PrettyPrint(recover = true)))
    val compiler = new CompilerFromParticles(splicedParticles)

    val state = compiler.parseAndTransform(input)

    state.output
  }

  override def toString = "Pretty Print"
}
