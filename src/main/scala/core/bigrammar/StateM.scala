package core.bigrammar

import core.bigrammar.BiGrammar.State

object StateM {
  def ret[T](value: T): StateM[T] = state => (state, value)
}

trait StateM[T] {
  def run(state: State): (State, T)

  def map(f: T => T): StateM[T] = state => {
    val (newState, value) = run(state)
    (newState, f(value))
  }

  def flatMap[U](f: T => StateM[U]): StateM[U] = state => {
    val (newState, value) = run(state)
    f(value).run(newState)
  }

  def apply(state: State) = run(state)
}
