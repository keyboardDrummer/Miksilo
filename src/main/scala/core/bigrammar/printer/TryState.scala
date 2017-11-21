package core.bigrammar.printer

import core.bigrammar.BiGrammar.State
import core.bigrammar.{StateFull, WithMapG}
import core.document.Empty
import core.responsiveDocument.ResponsiveDocument

import scala.util.{Failure, Success, Try}

object TryState {
  def ret[T](value: T): TryState[T] = state => Success((state, value))
  def fail[T](t: Throwable): TryState[T] = (state: State) => Failure(t)
  def fromStateM[T](stateM: StateFull[T]): TryState[T] = state => Success(stateM.run(state))
}

trait Functor[T] {
  def map[U](f: T => U): Functor[U]
}

trait TryState[To] {
  def run(state: State): Try[(State, To)]

  def flatMap[NewTo](function: To => TryState[NewTo]): TryState[NewTo] = state => {
    for {
      (state2, result1) <- run(state)
      result2 <- function(result1).run(state2)
    } yield result2
  }

  def mapError(pf: PartialFunction[Throwable, Throwable]): TryState[To] = (state: State) =>
    run(state).recoverWith(pf.andThen(t => Failure(t)))

  def recoverWith[U >: To](pf: PartialFunction[Throwable, TryState[U]]): TryState[U] = (state: State) =>
    run(state).recoverWith(pf.andThen(r => r.run(state)))

  def map[NewTo](function: To => NewTo): TryState[NewTo] = (state: State) =>
    run(state).map[(State, NewTo)](t => (t._1, function(t._2)))
}

trait Printer[T] {
  def write(from: WithMapG[T]): TryState[ResponsiveDocument]

  def map(function: ResponsiveDocument => ResponsiveDocument): Printer[T] =
    from => write(from).map(function)
}

object Printer {

  type NodePrinter = Printer[Any]
  type Result = Try[(State, ResponsiveDocument)]

  class NonePrintFailureException(e: Throwable) extends RuntimeException {
    override def toString = "failed toDocument with something different than a print failure: " + e.toString
  }

  def fail[T](inner: Any, depth: Int = 0): TryState[T] = (_: State) => Failure(RootError(depth, Empty, inner))
}