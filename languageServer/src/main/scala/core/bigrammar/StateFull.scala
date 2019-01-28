package core.bigrammar

import core.bigrammar.BiGrammar.State

object StateFull {
  def value[T](value: T): StateFull[T] = state => (state, value)

  def flattenMonads[T](monads: Seq[StateFull[T]]): StateFull[List[T]] = {
    monads.foldLeft(StateFull.value(List.empty[T]))(
      (result: StateFull[List[T]], monad: StateFull[T]) => result.flatMap(list => monad.map(element => element :: list)))
  }
}

class StateFullMap[T, U](origin: StateFull[T], f: T => U) extends StateFull[U] {
  override def run(state: State) = {
    val (newState, value) = origin.run(state)
    (newState, f(value))
  }
}

class StateFullFlatMap[T, U](origin: StateFull[T], f: T => StateFull[U]) extends StateFull[U] {
  override def run(state: State) = {
    val (newState, value) = origin.run(state)
    f(value).run(newState)
  }
}

trait StateFull[+T] { // TODO Can I get rid of the state monad by using Parser's Input type to store state?
  def run(state: State): (State, T)

  def map[U](f: T => U): StateFull[U] = new StateFullMap(this, f)

  def flatMap[U](f: T => StateFull[U]): StateFull[U] = new StateFullFlatMap(this, f)

  def apply(state: State): (State, T) = run(state)
}
