package core.transformation.sillyCodePieces

import core.transformation.{MetaObject, TransformationState}

trait ParticleWithPhase extends Particle {
  def transform(program: MetaObject, state: TransformationState)

  override def inject(state: TransformationState): Unit = {
    super.inject(state)
    state.compilerPhases ::= (() => transform(state.program, state))
  }
}
