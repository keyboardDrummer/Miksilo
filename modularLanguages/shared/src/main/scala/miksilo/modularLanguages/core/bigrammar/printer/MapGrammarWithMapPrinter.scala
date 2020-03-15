package miksilo.modularLanguages.core.bigrammar.printer

import miksilo.modularLanguages.core.bigrammar.BiGrammarToParser.AnyWithMap
import miksilo.modularLanguages.core.bigrammar.WithMap
import miksilo.modularLanguages.core.bigrammar.printer.Printer.NodePrinter
import miksilo.editorParser.responsiveDocument.ResponsiveDocument

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
