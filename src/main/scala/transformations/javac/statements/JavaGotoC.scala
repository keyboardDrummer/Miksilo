package transformations.javac.statements

import core.particles.{CompilationState, Particle, ParticleWithGrammar}

object JavaGotoC extends Particle {

  override def inject(state: CompilationState): Unit = {
    JustJavaGoto.inject(state)
    JustJavaLabel.inject(state)
    super.inject(state)
  }

  override def description: String = "Adds goto and label statements"
}
