package core.bigrammar.printer

import core.bigrammar.printer.Printer.NodePrinter
import core.bigrammar.{BiGrammar, WithMap}
import core.responsiveDocument.ResponsiveDocument

import scala.util.Failure

class NestPrinter[Value](grammar: BiGrammar[Value], inner: Printer[Value]) extends Printer[Value] {
  override def write(from: Value): TryState[ResponsiveDocument] = {
    state => inner.write(from).run(state).recoverWith(
      { case e: PrintError => Failure(NestedError((from, state), grammar, e))})
  }
}
