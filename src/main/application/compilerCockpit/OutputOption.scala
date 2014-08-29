package application.compilerCockpit

import core.transformation.TransformationState
import core.transformation.sillyCodePieces.Injector

object OutputOption {

  object Output

  def getOutput(state: TransformationState) = state.data.get(Output).collect({ case x: String => x})

  def setOutput(state: TransformationState, value: String) = state.data(Output) = value
}

trait OutputOption extends Injector {
}
