package core.deltas

import core.bigrammar.BiGrammarToParser
import core.language.node.Node
import core.language.{Compilation, Language}
import core.parsers.strings.{StringParserWriter, StringReader}
import core.smarts.FileDiagnostic
import util.SourceUtils

import scala.tools.nsc.interpreter.InputStream

object ParseUsingTextualGrammar extends DeltaWithPhase with StringParserWriter {

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val parser: Parser[Any] = parserProp.get(compilation)

    val uri = compilation.rootFile.get
    val inputStream = compilation.fileSystem.getFile(uri)
    val parseResult: ParseResult[Any] = parseStream(parser, inputStream)
    parseResult match {
      case success: ParseSuccess[_] =>
        compilation.program = success.result.asInstanceOf[Node]
        compilation.program.startOfUri = Some(uri)
      case failure: ParseFailure[_] =>
        compilation.diagnostics ++= List(FileDiagnostic(uri, DiagnosticUtil.getDiagnosticFromParseFailure(failure)))
    }
  }

  def parseStream(parser: Parser[Any], input: InputStream): ParseResult[Any] = {
    val reader = new StringReader(SourceUtils.streamToString(input))
    parser.parseWholeInput(reader)
  }

  val parserProp = new Property[Parser[Any]](null)

  override def inject(language: Language): Unit = {
    super.inject(language)
    parserProp.add(language, BiGrammarToParser.toParser(language.grammars.root))
  }

  override def description: String = "Parses the input file using a textual grammar."

  override def dependencies: Set[Contract] = Set.empty
}

