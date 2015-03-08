package core.particles

trait ParticleWithPhase extends Particle {
  def transform(program: MetaObject, state: CompilationState)

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    state.compilerPhases ::= (() => transform(state.program, state))
  }
}
