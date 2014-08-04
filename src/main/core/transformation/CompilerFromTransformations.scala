package core.transformation

import java.io.OutputStream

import core.exceptions.TransformationDependencyViolation
import core.grammar.ParseException
import util.FileNameUtils

import scala.reflect.io.{Directory, File, Path}

class CompilerFromTransformations(val transformations: Seq[Injector]) extends Compiler {

  validateDependencies(transformations)
  val manager = new TransformationsToPackrat()
  val parser = manager.buildParser(transformations.collect({ case t: GrammarTransformation => t}).reverse)

  def compile(input: File, outputDirectory: Directory) {
    val inputStream = File(input).slurp()
    outputDirectory.createDirectory()
    val byteCodeFile = File.apply(outputDirectory / Path(FileNameUtils.removeExtension(input.name)).addExtension("class"))
    val writer = byteCodeFile.outputStream(append = false)
    compile(inputStream, writer)
    writer.close()
  }

  private def compile(input: String, output: OutputStream) {
    val transformationsWithPrint = transformations ++ Seq(new PrintByteCodeToOutputStreamTransformation(output))
    val program = parse(input)

    new Transformer(transformationsWithPrint).transform(program)
  }

  def parse(input: String): MetaObject = {
    val parseResult = parser(input)
    if (!parseResult.successful)
      throw new ParseException(parseResult.toString)
    parseResult.get.asInstanceOf[MetaObject]
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
