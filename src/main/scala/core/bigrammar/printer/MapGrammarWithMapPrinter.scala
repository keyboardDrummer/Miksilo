package core.bigrammar.printer

import core.bigrammar.BiGrammarToParser.WithMap
import core.bigrammar.WithMapG
import core.bigrammar.printer.Printer.NodePrinter
import core.responsiveDocument.ResponsiveDocument

class MapGrammarWithMapPrinter(inner: NodePrinter, deconstruct: WithMap => Option[WithMap]) extends NodePrinter {
  private val fail = Printer.fail("could not deconstruct value")
  override def write(from: WithMapG[Any]): TryState[ResponsiveDocument] = {
    val deconstructed = deconstruct(from)
    for {
      deconstructedValue <- deconstructed match {
        case Some(v) => TryState.value(v)
        case _ => fail
      }
      result <- inner.write(deconstructedValue).mapError { case e: PrintError => MappedError(x => x, e) }
    } yield result
  }
}
