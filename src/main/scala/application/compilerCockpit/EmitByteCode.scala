package application.compilerCockpit

import java.io.InputStream

import transformations.bytecode.PrintByteCode

object EmitByteCode extends CompileOption {

  override def perform(cockpit: CompilerCockpit, input: InputStream): String = {
    val state = cockpit.compiler.parseAndTransform(input)
    val bytes = PrintByteCode.getBytes(state.program, state).toArray
    PrintByteCode.printBytes(bytes)
  }

  override def toString = "Emit ByteCode"
}
