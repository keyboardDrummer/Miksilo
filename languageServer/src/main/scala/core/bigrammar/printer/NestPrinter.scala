package core.bigrammar.printer

import core.bigrammar.printer.Printer.NodePrinter
import core.bigrammar.{BiGrammar, WithMapG}
import core.responsiveDocument.ResponsiveDocument

import scala.util.Failure

class NestPrinter(grammar: BiGrammar, inner: NodePrinter) extends NodePrinter {
  override def write(from: WithMapG[Any]): TryState[ResponsiveDocument] = {
    state => inner.write(from).run(state).recoverWith(
      { case e: PrintError => Failure(NestedError((from, state), grammar, e))})
  }
}
