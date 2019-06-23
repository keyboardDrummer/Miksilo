package core.bigrammar.printer

import core.bigrammar.BiGrammar.State
import core.bigrammar.printer.Printer.TryState
import core.bigrammar.{BiGrammar, WithMap}
import core.responsiveDocument.ResponsiveDocument

import scala.util.{Failure, Try}

trait Printer[T] {
  def write(from: WithMap[T], state: State): TryState[ResponsiveDocument]

  def map(function: ResponsiveDocument => ResponsiveDocument): Printer[T] =
    (from, state) => write(from, state).map(t => (t._1, function(t._2)))
}

object Printer {

  type TryState[To] = Try[(State, To)]
  type NodePrinter = Printer[Any]
  type Result = Try[(BiGrammar.State, ResponsiveDocument)]

  class NonePrintFailureException(e: Throwable) extends RuntimeException {
    override def toString = "failed toDocument with something different than a print failure: " + e.toString
  }

  def fail[T](message: Any): TryState[T] = Failure(RootError(message))
  def fail[T](message: Any, depth: Int): TryState[T] = Failure(NegativeDepthRootError(message, depth))
}