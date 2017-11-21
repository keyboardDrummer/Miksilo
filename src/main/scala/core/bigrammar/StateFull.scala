package core.bigrammar

import core.bigrammar.BiGrammar.State

object StateFull {
  def value[T](value: T): StateFull[T] = state => (state, value)
}

trait StateFull[T] {
  def run(state: State): (State, T)

  def map(f: T => T): StateFull[T] = state => {
    val (newState, value) = run(state)
    (newState, f(value))
  }

  def flatMap[U](f: T => StateFull[U]): StateFull[U] = state => {
    val (newState, value) = run(state)
    f(value).run(newState)
  }

  def apply(state: State) = run(state)
}
