package core.language

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.exceptions.BadInputException
import core.language.node.Node
import core.smarts.ConstraintBuilder

import scala.collection.mutable
import scala.reflect.io.File
import scala.util.Try

class Language {

  val data: mutable.Map[Any, Any] = mutable.Map.empty
  val grammars = new LanguageGrammars
  var compilerPhases: List[Phase] = List.empty
  var buildParser: () => (InputStream => Try[Node]) = () => null
  var collectConstraints: (Compilation, ConstraintBuilder) => Unit = _
  lazy val parser: InputStream => Try[Node] = buildParser()

  def parse(input: InputStream): Try[Node] = parser(input)

  def parseAndTransform(input: File): Compilation = {
    parseAndTransform(input.inputStream())
  }

  def compile(inputStream: InputStream, outputFile: File): Compilation = {
    val state: Compilation = parseAndTransform(inputStream)

    PrintByteCodeToOutputDirectory.perform(outputFile, state)
    state
  }

  def transform(program: Node): Compilation = {
    val state = new Compilation(this)
    state.program = program
    state.runPhases()
    state
  }

  def stringToInputStream(input: String) = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))

  def parseAndTransform(input: InputStream): Compilation = {
    val compilation = new Compilation(this)
    compilation.program = parse(input).get
    compilation.runPhases()
    compilation
  }
}

case class Phase(key: Delta, action: Compilation => Unit)

case class ParseException(message: String) extends BadInputException {
  override def toString: String = message
}

object NoSourceException extends BadInputException
