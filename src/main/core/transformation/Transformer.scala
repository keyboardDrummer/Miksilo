package core.transformation

import core.modularProgram.PieceCombiner
import core.transformation.sillyCodePieces.Injector

class Transformer(val injectors: Seq[Injector]) {
  def transform(program: MetaObject) = {
    val state = new TransformationState
    state.program = program
    PieceCombiner.combineAndExecute(state, injectors.reverse)
    program
  }
}
