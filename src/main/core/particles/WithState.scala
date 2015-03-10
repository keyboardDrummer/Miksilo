package core.particles

import scala.collection.mutable

trait WithState {
  type State
  class ClassRegistry[Registration] extends mutable.HashMap[Any, Registration]

  def createState: State
  def getState(state: CompilationState) = state.data.getOrElseUpdate(this, createState).asInstanceOf[State]
}
