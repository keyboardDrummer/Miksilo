package core.bigrammar.printer

import core.bigrammar.BiGrammar.State
import core.bigrammar.printer.Printer.{NodePrinter, TryState}
import core.bigrammar.{BiGrammar, WithMap}
import core.responsiveDocument.ResponsiveDocument

import scala.util.Failure

class NestPrinter(grammar: BiGrammar, inner: NodePrinter) extends NodePrinter {
  override def write(from: WithMap[Any], state: State): TryState[ResponsiveDocument] = {
    inner.write(from, state).recoverWith(
      { case e: PrintError => Failure(NestedError((from, state), grammar, e))})
  }
}
