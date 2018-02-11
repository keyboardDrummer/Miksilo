package core.deltas

import core.language.Compilation

trait WithCompilationState {
  type State

  def createState: State
  def getState(compilation: Compilation): State = compilation.state.getOrElseUpdate(this, createState).asInstanceOf[State]
}
