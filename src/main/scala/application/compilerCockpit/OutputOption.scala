package application.compilerCockpit

import core.particles.CompilationState

object OutputOption {

  object Output

  def getOutput(state: CompilationState) = state.data.get(Output).collect({ case x: String => x})

  def setOutput(state: CompilationState, value: String) = state.data(Output) = value
}

trait OutputOption {
  def handleOutput(output: String)
}
