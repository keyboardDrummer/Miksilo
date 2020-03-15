package miksilo.modularLanguages.core.bigrammar.printer

import miksilo.modularLanguages.core.bigrammar.{BiGrammar, WithMap}
import miksilo.modularLanguages.core.bigrammar.printer.Printer.NodePrinter
import miksilo.editorParser.responsiveDocument.ResponsiveDocument

import scala.util.Failure

class NestPrinter(grammar: BiGrammar, inner: NodePrinter) extends NodePrinter {
  override def write(from: WithMap[Any]): TryState[ResponsiveDocument] = {
    state => inner.write(from).run(state).recoverWith(
      { case e: PrintError => Failure(NestedError((from, state), grammar, e))})
  }
}
