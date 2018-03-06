package application.compilerCockpit

import java.io.InputStream

import deltas.bytecode.PrintByteCode

object EmitByteCode extends CompileOption {

  override def initialize(sandbox: LanguageSandbox): Unit = {}

  override def run(sandbox: LanguageSandbox, input: InputStream): TextWithGrammar = {
    val compilation = sandbox.language.parseAndTransform(input)
    val bytes = PrintByteCode.getBytes(compilation, compilation.program).toArray
    TextWithGrammar(PrintByteCode.printBytes(bytes))
  }

  override def toString = "Emit ByteCode"
}
