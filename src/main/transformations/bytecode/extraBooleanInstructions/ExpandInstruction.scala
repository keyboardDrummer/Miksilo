package transformations.bytecode.extraBooleanInstructions

import core.transformation.{Particle, Contract, CompilationState, MetaObject}

trait ExpandInstruction extends Particle {
  def key: Any

  override def dependencies: Set[Contract] = Set(ExpandInstructionsC)

  def expand(instruction: MetaObject, state: CompilationState) : Seq[MetaObject]

  override def inject(state: CompilationState): Unit = {
    ExpandInstructionsC.getState(state).expandInstruction.put(key, i => expand(i, state))
  }
}
