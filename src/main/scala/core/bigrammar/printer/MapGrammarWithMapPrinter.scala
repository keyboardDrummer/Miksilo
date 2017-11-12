package core.bigrammar.printer

import core.bigrammar.BiGrammarToGrammar.WithMap
import core.bigrammar.WithMapG
import core.bigrammar.printer.TryState.{NodePrinter, State, fail}
import core.responsiveDocument.ResponsiveDocument

import scala.util.{Failure, Try}

class MapGrammarWithMapPrinter(inner: NodePrinter, deconstruct: WithMap => Option[WithMap]) extends NodePrinter {
  override def write(from: WithMapG[Any], state: State): Try[(State, ResponsiveDocument)] = {

    for {
      deconstructedValue <- deconstruct(from).fold[Try[WithMapG[Any]]](
        fail("could not deconstruct value"))(
        r => Try(r))
      result <- inner.write(deconstructedValue, state).recoverWith { case e: PrintError => Failure(e.mapPartial(x => x)) }
    } yield result
  }
}
