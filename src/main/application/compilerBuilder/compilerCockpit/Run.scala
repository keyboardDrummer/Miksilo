package application.compilerBuilder.compilerCockpit

import core.transformation.TransformationState
import util.TestUtils

object Run extends CompileOption {
  override def leave(state: TransformationState): Unit = {
    val result = TestUtils.runByteCode("SimpleForLoop", state.program)
    OutputOption.setOutput(state, result)
  }

  override def toString = "Compile and run"
}
