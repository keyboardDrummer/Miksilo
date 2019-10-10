package core.bigrammar.printer

import core.bigrammar.BiGrammarToParser.AnyWithMap
import core.bigrammar.WithMap
import core.bigrammar.printer.Printer.NodePrinter
import core.responsiveDocument.ResponsiveDocument

class MapGrammarWithMapPrinter(inner: NodePrinter, deconstruct: AnyWithMap => Option[AnyWithMap]) extends NodePrinter {
  private val fail = Printer.fail("could not deconstruct value")
  override def write(from: WithMap[Any]): TryState[ResponsiveDocument] = {
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
