package core.language

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets

import com.typesafe.scalalogging.LazyLogging
import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.PathRoot
import core.language.exceptions.BadInputException
import core.language.node.Node
import core.smarts.ConstraintBuilder
import deltas.ConstraintSkeleton

import scala.collection.mutable
import scala.reflect.io.File

class Language extends LazyLogging {

  val data: mutable.Map[Any, Any] = mutable.Map.empty
  val grammars = new LanguageGrammars
  var compilerPhases: List[Phase] = List.empty
  var collectConstraints: (Compilation, ConstraintBuilder) => Unit = (compilation, builder) => {
    ConstraintSkeleton.constraints(compilation, builder, PathRoot(compilation.program),
      builder.newScope(debugName = "rootScope"))
  }

  var extraCompileOptions: List[CustomCommand] = List.empty

  def compileString(input: String): Compilation = {
    compileStream(stringToInputStream(input))
  }

  def compileFile(input: File): Compilation = {
    compileStream(input.inputStream())
  }

  def compileFileToOutputFile(inputStream: InputStream, outputFile: File): Compilation = {
    val compilation = compileStream(inputStream)
    PrintByteCodeToOutputDirectory.perform(outputFile, compilation)
    compilation
  }

  def compileAst(program: Node): Compilation = {
    val compilation = Compilation.fromAst(this, program)
    compilation.program = program
    compilation.runPhases()
    compilation
  }

  def stringToInputStream(input: String) = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))

  def compileStream(input: InputStream): Compilation = {
    val compilation = Compilation.singleFile(this, input)
    compilation.runPhases()
    compilation
  }
}

case class Phase(key: Delta, action: Compilation => Unit)

case class ParseException(message: String) extends BadInputException {
  override def toString: String = message
}

object NoSourceException extends BadInputException
