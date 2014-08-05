package core.transformation.sillyCodePieces

import core.transformation.{MetaObject, TransformationState}

trait ProgramTransformation extends Injector {
  def transform(program: MetaObject, state: TransformationState)

  override def leave(state: TransformationState): Unit = {
    transform(state.program, state)
  }
}
