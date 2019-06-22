package core.bigrammar.printer

import core.bigrammar.BiGrammar.State

import scala.util.{Failure, Success, Try}

object TryState {
  def value[T](value: T): TryState[T] = state => Success((state, value))
  def fail[T](t: Throwable): TryState[T] = (state: State) => Failure(t)
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