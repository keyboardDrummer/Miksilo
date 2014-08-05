package core.transformation

import core.modularProgram.PieceOfCode
import transformations.bytecode.PrintByteCode

import scala.reflect.io.{Directory, File, Path}

class PrintByteCodeToOutputDirectory(fileName: String, outputDirectory: Directory) extends PieceOfCode[TransformationState] {

  override def leave(state: TransformationState): Unit = {
    outputDirectory.createDirectory()
    val byteCodeFile = File.apply(outputDirectory / Path(fileName).addExtension("class"))
    val writer = byteCodeFile.outputStream(append = false)
    val bytes = PrintByteCode.getBytes(state.program, state).toArray
    writer.write(bytes)
    writer.close()
  }

  override def enter(state: TransformationState): Unit = {}
}
