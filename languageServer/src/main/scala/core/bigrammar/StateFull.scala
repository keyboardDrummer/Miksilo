package core.bigrammar

import core.bigrammar.BiGrammar.State

object StateFull {
  def value[T](value: T): StateFull[T] = state => (state, value)

  def flattenMonads[T](monads: Seq[StateFull[T]]): StateFull[List[T]] = {
    monads.foldLeft(StateFull.value(List.empty[T]))(
      (result: StateFull[List[T]], monad: StateFull[T]) => result.flatMap(list => monad.map(element => element :: list)))
  }
}

trait StateFull[+T] { // TODO Can I get rid of the state monad by using Parser's Input type to store state?
  def run(state: State): (State, T)

  def map[U](f: T => U): StateFull[U] = state => {
    val (newState, value) = run(state)
    (newState, f(value))
  }

  def flatMap[U](f: T => StateFull[U]): StateFull[U] = state => {
    val (newState, value) = run(state)
    f(value).run(newState)
  }

  def apply(state: State): (State, T) = run(state)
}
