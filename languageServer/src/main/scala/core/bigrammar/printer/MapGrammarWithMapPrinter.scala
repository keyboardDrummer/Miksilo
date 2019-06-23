package core.bigrammar.printer

import core.bigrammar.BiGrammar.State
import core.bigrammar.BiGrammarToParser.AnyWithMap
import core.bigrammar.WithMap
import core.bigrammar.printer.Printer.{NodePrinter, TryState}
import core.responsiveDocument.ResponsiveDocument

import scala.util.{Failure, Success}

class MapGrammarWithMapPrinter(inner: NodePrinter, deconstruct: AnyWithMap => Option[AnyWithMap]) extends NodePrinter {
  private val fail = Printer.fail("could not deconstruct value")
  override def write(from: WithMap[Any], state: State): TryState[ResponsiveDocument] = {
    val deconstructed = deconstruct(from)
    deconstructed match {
      case None => fail
      case Some(deconstructedValue) =>
        inner.write(deconstructedValue, state).recoverWith({ case e: PrintError => Failure(MappedError(x => x, e)) })
    }
  }
}
