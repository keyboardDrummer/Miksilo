package application.compilerBuilder.compilerCockpit

import core.transformation.TransformationState
import core.transformation.sillyCodePieces.Injector

object OutputOption
{
  object Output

  def getOutput(state: TransformationState) = state.data(Output).asInstanceOf[String]
  def setOutput(state: TransformationState, value: String) = state.data(Output) = value
}

trait OutputOption extends Injector
{
}
