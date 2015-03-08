package core.transformation

trait ParticleWithState {
  type State
  class ClassRegistry[Registration] extends mutable.HashMap[Any, Registration]

  def createState: State
  def getState(state: CompilationState) = state.data.getOrElseUpdate(this, createState).asInstanceOf[State]
}
