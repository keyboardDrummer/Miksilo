package core.bigrammar.printer

import core.bigrammar.printer.TryState.{NodePrinter, Result, State}
import core.bigrammar.{BiGrammar, WithMapG}

import scala.util.Failure

class NestPrinter(grammar: BiGrammar, inner: NodePrinter) extends NodePrinter {
  override def write(from: WithMapG[Any], state: State): Result = {
    inner.write(from, state).recoverWith(
      { case e: PrintError => Failure(NestedError((from, state), grammar, e))})
  }
}
