package core.particles

import scala.collection.mutable

trait WithState {
  type State

  type ClassRegistry[Registration] = mutable.HashMap[Any, Registration]
  def createState: State
  def getState(state: CompilationState) = state.data.getOrElseUpdate(this, createState).asInstanceOf[State]
}
