package core.deltas

import deltas.bytecode.PrintByteCode
import util.FileNameUtils

import scala.reflect.io.{Directory, File, Path}

object PrintByteCodeToOutputDirectory {

  def perform(inputFile: File, outputDirectory: Directory, state: Compilation): Unit = {
    val bytes = PrintByteCode.getBytes(state.program, state.language).toArray
    val fileName = FileNameUtils.removeExtension(inputFile.name)
    outputDirectory.createDirectory()
    val byteCodeFile = File.apply(outputDirectory / Path(fileName).addExtension("class"))
    val writer = byteCodeFile.outputStream(append = false)
    writer.write(bytes)
    writer.close()
  }
}
