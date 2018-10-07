package core.deltas

import core.bigrammar.BiGrammarToParser
import core.language.node.{Node, SourceRange}
import core.language.{Compilation, Language}
import langserver.types.{Diagnostic, DiagnosticSeverity}
import languageServer.HumanPosition
import util.SourceUtils

import scala.tools.nsc.interpreter.InputStream
import scala.util.parsing.input.CharArrayReader

object DiagnosticUtil {

  private val rowColumnRegex = """\[(\d*)\.(\d*)\] failure: (.*)\n\n""".r
  def getDiagnosticFromParseException(message: String): Diagnostic = {
    val messageMatch = rowColumnRegex.findFirstMatchIn(message).get
    val row = messageMatch.group(1).toInt
    val column = messageMatch.group(2).toInt
    Diagnostic(SourceRange(HumanPosition(row, column), HumanPosition(row, column + 1)), Some(DiagnosticSeverity.Error), None, None, messageMatch.group(3))
  }
}

object ParseUsingTextualGrammar extends DeltaWithPhase {


  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val parser: BiGrammarToParser.PackratParser[Any] = parserProp.get(compilation)

    val input = compilation.fileSystem.getFile(compilation.rootFile.get)
    val parseResult: BiGrammarToParser.ParseResult[Any] = parseStream(parser, input)
    if (parseResult.successful)
      compilation.program = parseResult.get.asInstanceOf[Node]
    else
      compilation.diagnostics ++= List(DiagnosticUtil.getDiagnosticFromParseException(parseResult.toString))
  }

  def parseStream(parser: BiGrammarToParser.PackratParser[Any], input: InputStream): BiGrammarToParser.ParseResult[Any] = {
    val reader = new CharArrayReader(SourceUtils.streamToString(input).mkString.toCharArray)
    if (reader.source.length() == 0) {
      ???
    }

    val parseResult: BiGrammarToParser.ParseResult[Any] = parser(reader)
    parseResult
  }

  val parserProp = new Property[BiGrammarToParser.PackratParser[Any]](null)

  override def inject(language: Language): Unit = {
    super.inject(language)
    parserProp.add(language, BiGrammarToParser.toParser(language.grammars.root))
  }

  override def description: String = "Parses the input file using a textual grammar."

  override def dependencies: Set[Contract] = Set.empty
}
