package miksilo.modularLanguages.core.deltas

import miksilo.modularLanguages.core.deltas.path.PathRoot
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode

import scala.reflect.io.File

object PrintByteCodeToOutputDirectory {

  def perform(outputFile: File, compilation: Compilation): Unit = {
    val bytes = PrintByteCode.getBytes(compilation, compilation.program.asInstanceOf[PathRoot].current).toArray
    outputFile.createFile()
    val writer = outputFile.outputStream(append = false)
    writer.write(bytes)
    writer.close()
  }
}
