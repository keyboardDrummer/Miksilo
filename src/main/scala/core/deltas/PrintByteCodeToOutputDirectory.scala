package core.deltas

import deltas.bytecode.PrintByteCode

import scala.reflect.io.File

object PrintByteCodeToOutputDirectory {

  def perform(outputFile: File, state: Compilation): Unit = {
    val bytes = PrintByteCode.getBytes(state.program, state.language).toArray
    outputFile.createFile()
    val writer = outputFile.outputStream(append = false)
    writer.write(bytes)
    writer.close()
  }
}
