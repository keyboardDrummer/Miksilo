package core.language

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets

import com.typesafe.scalalogging.LazyLogging
import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.exceptions.BadInputException
import core.language.node.{Node, SourceRange}
import core.smarts.ConstraintBuilder
import langserver.types.{Diagnostic, DiagnosticSeverity}
import languageServer.HumanPosition

import scala.collection.mutable
import scala.reflect.io.File
import scala.util.{Failure, Success, Try}

class Language extends LazyLogging {

  val data: mutable.Map[Any, Any] = mutable.Map.empty
  val grammars = new LanguageGrammars
  var compilerPhases: List[Phase] = List.empty
  var buildParser: () => (InputStream => Try[Node]) = () => null
  var collectConstraints: (Compilation, ConstraintBuilder) => Unit = _
  lazy val parser: InputStream => Try[Node] = buildParser()
  var extraCompileOptions: List[CustomCommand] = List.empty

  def parse(input: InputStream): Try[Node] = parser(input)

  def parseIntoDiagnostic(input: InputStream): Either[Node, List[Diagnostic]] = {
    val parseResult: Try[Node] = parse(input)
    parseResult match {
      case Failure(NoSourceException) => Right(List.empty)
      case Failure(ParseException(message)) =>
        Right(List(getDiagnosticFromParseException(message)))
      case Success(node) => Left(node)
      case Failure(e) =>
        logger.error("Received unknown error during parsing: " + e.toString)
        Right(List.empty)
    }
  }

  private val rowColumnRegex = """\[(\d*)\.(\d*)\] failure: (.*)\n\n""".r
  private def getDiagnosticFromParseException(message: String): Diagnostic = {
    val messageMatch = rowColumnRegex.findFirstMatchIn(message).get
    val row = messageMatch.group(1).toInt
    val column = messageMatch.group(2).toInt
    Diagnostic(SourceRange(HumanPosition(row, column), HumanPosition(row, column + 1)), Some(DiagnosticSeverity.Error), None, None, messageMatch.group(3))
  }

  def parseAndTransform(input: File): Compilation = {
    parseAndTransform(input.inputStream())
  }

  def compile(inputStream: InputStream, outputFile: File): Compilation = {
    val state: Compilation = parseAndTransform(inputStream)

    PrintByteCodeToOutputDirectory.perform(outputFile, state)
    state
  }

  def transform(program: Node): Compilation = {
    val state = Compilation.fromAst(this, program)
    state.program = program
    state.runPhases()
    state
  }

  def stringToInputStream(input: String) = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))

  def parseAndTransform(input: InputStream): Compilation = {
    val compilation = Compilation.singleFile(this, input)
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
