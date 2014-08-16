package transformations.bytecode.extraBooleanInstructions

import core.transformation.{Contract, TransformationState, MetaObject}
import core.transformation.sillyCodePieces.Injector

trait ExpandInstruction extends Injector {
  def key: Any

  override def dependencies: Set[Contract] = Set(ExpandInstructionsC)

  def expand(instruction: MetaObject, state: TransformationState) : Seq[MetaObject]

  override def inject(state: TransformationState): Unit = {
    ExpandInstructionsC.getState(state).expandInstruction.put(key, i => expand(i, state))
  }
}
