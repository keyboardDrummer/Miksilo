package application.compilerCockpit

import java.io.InputStream

import deltas.bytecode.PrintByteCode

object EmitByteCode extends CompileOption {

  override def perform(cockpit: LanguageSandbox, input: InputStream): TextWithGrammar = {
    val compilation = cockpit.language.parseAndTransform(input)
    val bytes = PrintByteCode.getBytes(compilation, compilation.program).toArray
    TextWithGrammar(PrintByteCode.printBytes(bytes))
  }

  override def toString = "Emit ByteCode"
}
