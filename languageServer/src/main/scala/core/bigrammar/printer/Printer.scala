package core.bigrammar.printer

import core.bigrammar.{BiGrammar, WithMap}
import core.responsiveDocument.ResponsiveDocument

import scala.util.Try

trait Printer[-T] {
  def write(from: T): TryState[ResponsiveDocument]

  def map(function: ResponsiveDocument => ResponsiveDocument): Printer[T] =
    from => write(from).map(function)
}

object Printer {

  type NodePrinter = Printer[Any]
  type Result = Try[(BiGrammar.State, ResponsiveDocument)]

  class NonePrintFailureException(e: Throwable) extends RuntimeException {
    override def toString = "failed toDocument with something different than a print failure: " + e.toString
  }

  def fail[T](message: Any): TryState[T] = TryState.fail(RootError(message))
  def fail[T](message: Any, depth: Int): TryState[T] = TryState.fail(NegativeDepthRootError(message, depth))
}