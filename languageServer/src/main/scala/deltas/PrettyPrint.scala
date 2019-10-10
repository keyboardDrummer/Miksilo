package deltas

import core.bigrammar.BiGrammar
import core.bigrammar.printer.{BiGrammarToPrinter, PrintError}
import core.deltas.{Contract, Delta}
import core.language.{Language, Phase}
import core.responsiveDocument.ResponsiveDocument

import scala.util.Try

case class PrettyPrint(recover: Boolean = false) extends Delta
{
  override def inject(language: Language): Unit = {
    val foundGrammar = language.grammars.root
    language.data(this) = foundGrammar.deepClone

    language.compilerPhases = List(Phase(this, compilation => {
      val grammar = getOutputGrammar(language)
      val documentTry: Try[ResponsiveDocument] = Try(BiGrammarToPrinter.toDocument(compilation.program, grammar))
      val documentTryWithOptionalRecover: Try[ResponsiveDocument] = if (recover) {
        documentTry.recover({ case e: PrintError => e.toDocumentWithPartial })
      }
      else {
        documentTry
      }
      val document: ResponsiveDocument = documentTryWithOptionalRecover.get
      compilation.output = document.renderString()
    }))
  }

  override def description: String = "Prints the program by generating a pretty printer from its grammar."

  def getOutputGrammar(language: Language): BiGrammar[_] = language.data(this).asInstanceOf[BiGrammar[_]]

  override def dependencies: Set[Contract] = Set.empty
}
