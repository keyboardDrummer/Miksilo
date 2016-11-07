package transformations.javac.statements

import core.particles.{CompilationState, Delta, DeltaWithGrammar}

object JavaGotoC extends Delta {

  override def inject(state: CompilationState): Unit = {
    JustJavaGoto.inject(state)
    JustJavaLabel.inject(state)
    super.inject(state)
  }

  override def description: String = "Adds goto and label statements"
}
