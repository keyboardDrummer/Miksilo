package application.compilerCockpit

import java.io.InputStream

import deltas.bytecode.PrintByteCode

object EmitByteCode extends CompileOption {

  override def perform(cockpit: CompilerCockpit, input: InputStream): TextWithGrammar = {
    val state = cockpit.language.parseAndTransform(input)
    val bytes = PrintByteCode.getBytes(state.program, state.language).toArray
    TextWithGrammar(PrintByteCode.printBytes(bytes))
  }

  override def toString = "Emit ByteCode"
}
