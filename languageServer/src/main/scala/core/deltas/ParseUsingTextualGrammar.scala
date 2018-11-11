package core.deltas

import core.bigrammar.BiGrammarToParser
import core.language.node.{Node, SourceRange}
import core.language.{Compilation, Language}
import core.parsers.StringReader
import core.smarts.FileDiagnostic
import langserver.types.{Diagnostic, DiagnosticSeverity}
import languageServer.HumanPosition
import util.SourceUtils

import scala.tools.nsc.interpreter.InputStream

object ParseUsingTextualGrammar extends DeltaWithPhase {

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val parser: BiGrammarToParser.Parser[Any] = parserProp.get(compilation)

    val uri = compilation.rootFile.get
    val inputStream = compilation.fileSystem.getFile(uri)
    val parseResult: BiGrammarToParser.ParseResult[Any] = parseStream(parser, inputStream)
    parseResult match {
      case success: BiGrammarToParser.ParseSuccess[_] =>
        compilation.program = success.result.asInstanceOf[Node]
        compilation.program.startOfUri = Some(uri)
      case failure: BiGrammarToParser.ParseFailure[_] =>
        compilation.diagnostics ++= List(FileDiagnostic(uri, DiagnosticUtil.getDiagnosticFromParseFailure(failure)))
    }
  }

  def parseStream(parser: BiGrammarToParser.Parser[Any], input: InputStream): BiGrammarToParser.ParseResult[Any] = {
    val reader = new StringReader(SourceUtils.streamToString(input))
    parser.parseWhole(reader)
  }

  val parserProp = new Property[BiGrammarToParser.Parser[Any]](null)

  override def inject(language: Language): Unit = {
    super.inject(language)
    parserProp.add(language, BiGrammarToParser.toParser(language.grammars.root))
  }

  override def description: String = "Parses the input file using a textual grammar."

  override def dependencies: Set[Contract] = Set.empty
}

object DiagnosticUtil {

  private val rowColumnRegex = """\[(\d*)\.(\d*)\] failure: ((.|\n)*)\n\n""".r

  def getDiagnosticFromParseFailure(failure: BiGrammarToParser.ParseFailure[Any]): Diagnostic = {
    val row = failure.remainder.position.line
    val column = failure.remainder.position.column
    Diagnostic(SourceRange(HumanPosition(row, column), HumanPosition(row, column + 1)), Some(DiagnosticSeverity.Error), None, None, failure.message)
  }

  def getDiagnosticFromParseException(message: String): Diagnostic = {
    try {
      val messageMatch = rowColumnRegex.findFirstMatchIn(message).get
      val row = messageMatch.group(1).toInt
      val column = messageMatch.group(2).toInt
      Diagnostic(SourceRange(HumanPosition(row, column), HumanPosition(row, column + 1)), Some(DiagnosticSeverity.Error), None, None, messageMatch.group(3))

    } catch
    {
      case e: java.util.NoSuchElementException => throw new Exception("Failed to parse message " + message)
    }
  }
}