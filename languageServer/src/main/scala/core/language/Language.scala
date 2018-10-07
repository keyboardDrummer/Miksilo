package core.language

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets

import com.typesafe.scalalogging.LazyLogging
import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.exceptions.BadInputException
import core.language.node.Node
import core.smarts.ConstraintBuilder
import langserver.types.Diagnostic

import scala.collection.mutable
import scala.reflect.io.File

class Language extends LazyLogging {

  val data: mutable.Map[Any, Any] = mutable.Map.empty
  val grammars = new LanguageGrammars
  var compilerPhases: List[Phase] = List.empty
  var collectConstraints: (Compilation, ConstraintBuilder) => Unit = _
  var extraCompileOptions: List[CustomCommand] = List.empty
  lazy val justParse: Language = this //TODO correct

  def getParseDiagnostics(input: InputStream): List[Diagnostic] = {
    val compilation = justParse.compileFile(input)
    compilation.diagnostics
  }

  def compileString(input: String): Compilation = {
    compileFile(stringToInputStream(input))
  }

  def compileFile(input: File): Compilation = {
    compileFile(input.inputStream())
  }

  def compileFileToOutputFile(inputStream: InputStream, outputFile: File): Compilation = {
    val compilation = compileFile(inputStream)
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

  def compileFile(input: InputStream): Compilation = {
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
