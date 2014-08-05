package core.transformation

import core.exceptions.TransformationDependencyViolation
import core.modularProgram.{PieceCombiner, PieceOfCode}
import util.FileNameUtils

import scala.reflect.io.{Directory, File}

class CompilerFromTransformations(val transformations: Seq[Injector]) extends Compiler {

  validateDependencies(transformations)

  def compile(input: File, outputDirectory: Directory) {
    val readFilePiece = new ReadUsingGrammarTransformation(input)
    val fileName = FileNameUtils.removeExtension(input.name)
    val printByteCodePiece = new PrintByteCodeToOutputDirectory(fileName, outputDirectory)
    val pieces = Seq(readFilePiece) ++ transformations.map(i => new FromInjector(i)) ++ Seq(printByteCodePiece)
    PieceCombiner.combineAndExecute(new TransformationState(), pieces.reverse)
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

  class FromInjector(injector: Injector) extends PieceOfCode[TransformationState] {
    override def enter(state: TransformationState): Unit = {
      injector.inject(state)
      injector match {
        case grammarTransformation: GrammarTransformation => grammarTransformation.transformGrammars(state.grammarCatalogue)
        case _ =>
      }
    }

    override def leave(state: TransformationState): Unit = {
      injector match {
        case programTransformation: ProgramTransformation => programTransformation.transform(state.program, state)
        case _ =>
      }
    }
  }

}
