package core.transformation

import core.exceptions.TransformationDependencyViolation
import core.modularProgram.PieceCombiner
import core.transformation.grammars.ReadUsingGrammarTransformation
import core.transformation.sillyCodePieces.{Injector, ProgramTransformation}

import scala.reflect.io.{Directory, File}

class CompilerFromTransformations(val transformations: Seq[Injector]) extends Compiler {

  validateDependencies(transformations)

  def parseAndTransform(input: File): MetaObject = {
    val readFilePiece = ReadUsingGrammarTransformation
    val pieces = Seq(readFilePiece) ++ transformations
    val state = new TransformationState()
    state.data(ReadUsingGrammarTransformation.InputFile) = input
    PieceCombiner.combineAndExecute(state, pieces.reverse)
    state.program
  }

  def compile(input: File, outputDirectory: Directory) {
    val readFilePiece = ReadUsingGrammarTransformation
    val printByteCodePiece = PrintByteCodeToOutputDirectory
    val pieces = Seq(readFilePiece) ++ transformations ++ Seq(printByteCodePiece)
    val state = new TransformationState()
    state.data(PrintByteCodeToOutputDirectory.InputFile) = input
    state.data(PrintByteCodeToOutputDirectory.OutputDirectory) = outputDirectory
    state.data(ReadUsingGrammarTransformation.InputFile) = input
    PieceCombiner.combineAndExecute(state, pieces.reverse)
  }

  def validateDependencies(transformations: Seq[Injector]) = {
    var available = Set.empty[Contract]
    for (transformation <- transformations.reverse) {
      transformation.dependencies.collect({ case dependency: ProgramTransformation =>
        if (!available.contains(dependency))
          throw new TransformationDependencyViolation(dependency, transformation)
      })
      available += transformation
    }
  }
}
