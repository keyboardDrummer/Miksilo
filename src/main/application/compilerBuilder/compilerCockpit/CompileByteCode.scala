package application.compilerBuilder.compilerCockpit

import core.transformation.TransformationState
import transformations.bytecode.PrintByteCode

object CompileByteCode extends CompileOption {
  override def leave(state: TransformationState): Unit = {
    val bytes = PrintByteCode.getBytes(state.program, state).toArray
    OutputOption.setOutput(state, PrintByteCode.printBytes(bytes))
  }

  override def toString = "Compile to ByteCode"
}
