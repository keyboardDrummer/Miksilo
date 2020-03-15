package playground.application.compilerCockpit

import java.io.InputStream

import core.deltas.path.PathRoot
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode

object EmitByteCode extends CompileOption {

  override def initialize(sandbox: LanguageSandbox): Unit = {}

  override def run(sandbox: LanguageSandbox, input: String): TextWithGrammar = {
    val compilation = sandbox.language.compileString(input)
    val bytes = PrintByteCode.getBytes(compilation, compilation.program.asInstanceOf[PathRoot].current)
    TextWithGrammar(PrintByteCode.printBytes(bytes))
  }

  override def name = "Emit ByteCode"
}
