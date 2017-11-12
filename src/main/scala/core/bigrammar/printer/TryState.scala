package core.bigrammar.printer

import core.bigrammar.WithMapG
import core.responsiveDocument.ResponsiveDocument

import scala.util.{Failure, Try}
import TryState._
import core.document.Empty

trait TryState[From, To] {
  def write(from: WithMapG[From], state: State): Try[(State, To)]

  def map[NewTo](function: To => NewTo): TryState[From, NewTo] = (from: WithMapG[From], state: State) =>
    write(from, state).map[(State, NewTo)](t => (t._1, function(t._2)))
}

object TryState {

  type Printer[T] = TryState[T, ResponsiveDocument]
  type NodePrinter = Printer[Any]
  type State = Map[Any,Any]
  type Result = Try[(State, ResponsiveDocument)]

  class NonePrintFailureException(e: Throwable) extends RuntimeException {
    override def toString = "failed toDocument with something different than a print failure: " + e.toString
  }

  def fail(inner: Any, depth: Int = 0) = Failure(RootError(depth, Empty, inner))
}