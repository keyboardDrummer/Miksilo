package application.compilerCockpit

import java.io.InputStream

import core.bigrammar.printer.{BiGrammarToPrinter, PrintError, BiGrammarToPrinter$}
import core.bigrammar.{BiGrammarToGrammar, BiGrammar}
import core.particles.grammars.ProgramGrammar
import core.particles.{Phase, CompilationState, CompilerFromParticles, Delta}
import core.responsiveDocument.ResponsiveDocument

import scala.util.Try

case class PrettyPrint(recover: Boolean = false) extends Delta
{
  override def inject(state: CompilationState): Unit = {
    val foundGrammar = state.grammarCatalogue.find(ProgramGrammar)
    state.data(this) = foundGrammar.deepClone

    state.compilerPhases = List(new Phase(this.name, this.description, () => {
      val grammar = getOutputGrammar(state)
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

  def getOutputGrammar(state: CompilationState) = state.data(this).asInstanceOf[BiGrammar]
}

object PrettyPrintOption extends CompileOption {

  override def perform(cockpit: CompilerCockpit, input: InputStream): TextWithGrammar = {
    val prettyPrint = PrettyPrint(recover = true)
    val splicedParticles = cockpit.compiler.replace(MarkOutputGrammar,Seq(prettyPrint))
    val compiler = new CompilerFromParticles(splicedParticles)

    val state = compiler.parseAndTransform(input)
    val outputGrammar = prettyPrint.getOutputGrammar(state)
    TextWithGrammar(state.output, BiGrammarToGrammar.toGrammar(outputGrammar))
  }

  override def toString = "Pretty Print"
}
