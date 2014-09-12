package core.transformation.sillyCodePieces

import core.modularProgram.PieceOfCode
import core.transformation.{Contract, TransformationState}

trait Injector extends Contract with PieceOfCode[TransformationState] {
  override final def enter(state: TransformationState): Unit = {
    inject(state)
  }

  def inject(state: TransformationState) = {}

  final def dependencies2: Set[Injector] = dependencies.collect({case x: Injector => x})
}
