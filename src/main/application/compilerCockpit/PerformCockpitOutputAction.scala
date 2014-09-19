package application.compilerCockpit

import core.transformation.TransformationState
import core.transformation.sillyCodePieces.Injector

object PerformCockpitOutputAction extends Injector {

  def setState(state: TransformationState, injectors: Seq[Injector]) = state.data(this) = injectors

  def getState(state: TransformationState) = state.data(this).asInstanceOf[Seq[Injector]]

  override def inject(state: TransformationState): Unit = {
    for(particle <- getState(state))
      particle.enter(state)
  }

  override def leave(state: TransformationState): Unit = {
    for(particle <- getState(state))
      particle.leave(state)
  }
}
