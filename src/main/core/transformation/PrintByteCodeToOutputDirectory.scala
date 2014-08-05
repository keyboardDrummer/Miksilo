package core.transformation

import core.modularProgram.PieceOfCode
import transformations.bytecode.PrintByteCode
import util.FileNameUtils

import scala.reflect.io.{Directory, File, Path}

object PrintByteCodeToOutputDirectory extends PieceOfCode[TransformationState] {

  override def leave(state: TransformationState): Unit = {
    val input = state.data(InputFile).asInstanceOf[File]
    val outputDirectory = state.data(OutputDirectory).asInstanceOf[Directory]
    val fileName = FileNameUtils.removeExtension(input.name)
    outputDirectory.createDirectory()
    val byteCodeFile = File.apply(outputDirectory / Path(fileName).addExtension("class"))
    val writer = byteCodeFile.outputStream(append = false)
    val bytes = PrintByteCode.getBytes(state.program, state).toArray
    writer.write(bytes)
    writer.close()
  }

  override def enter(state: TransformationState): Unit = {}

  object InputFile

  object OutputDirectory

}
