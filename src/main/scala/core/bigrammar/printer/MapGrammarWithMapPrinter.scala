package core.bigrammar.printer

import core.bigrammar.BiGrammarToGrammar.WithMap
import core.bigrammar.WithMapG
import core.bigrammar.printer.Printer.NodePrinter
import core.responsiveDocument.ResponsiveDocument

class MapGrammarWithMapPrinter(inner: NodePrinter, deconstruct: WithMap => Option[WithMap]) extends NodePrinter {
  override def write(from: WithMapG[Any]): TryState[ResponsiveDocument] = {
    for {
      deconstructedValue <- deconstruct(from).fold[TryState[WithMapG[Any]]](
        Printer.fail("could not deconstruct value"))(
        r => TryState.value(r))
      result <- inner.write(deconstructedValue).mapError { case e: PrintError => MappedError(x => x, e) }
    } yield result
  }
}
