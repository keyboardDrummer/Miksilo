package core.deltas

import com.typesafe.scalalogging.LazyLogging
import core.bigrammar.BiGrammarToParser
import core.bigrammar.BiGrammarToParser.Reader
import core.bigrammar.printer.BiGrammarToPrinter
import core.language.node.Node
import core.language.{Compilation, DiagnosticUtil, Language}
import core.parsers.editorParsers.{SingleParseResult, StopFunction, TimeRatioStopFunction}
import core.parsers.sequences.SingleResultParser
import util.SourceUtils

import scala.tools.nsc.interpreter.InputStream

case class ParseUsingTextualGrammar(stopFunction: StopFunction = new TimeRatioStopFunction)
  extends DeltaWithPhase with LazyLogging {

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val parser = parserProp.get(compilation)

    val uri = compilation.rootFile.get
    val inputStream = compilation.fileSystem.getFile(uri)
    val time = System.currentTimeMillis()
    val parseResult: SingleParseResult[Node, BiGrammarToParser.Input] = parseStream(parser, inputStream)
    val parseTime = System.currentTimeMillis() - time
    logger.info(s"Parsing took ${parseTime}ms")
    parseResult.resultOption.foreach(program => {
      compilation.program = program
      compilation.program.startOfUri = Some(uri)
    })
    if (compilation.program == null) {
      compilation.stopped = true
    }
    if (!parseResult.successful) {
      val diagnostics = DiagnosticUtil.getDiagnosticsFromParseFailures(uri, parseResult.errors)
      compilation.addDiagnosticsWithFixes(diagnostics)
    }
  }

  def parseStream[T](parser: SingleResultParser[T, BiGrammarToParser.Input], input: InputStream): SingleParseResult[T, BiGrammarToParser.Input] = {
    parser.parse(new Reader(SourceUtils.streamToString(input)), stopFunction)
  }

  val parserProp = new Property[SingleResultParser[Node, BiGrammarToParser.Input]](null)

  override def inject(language: Language): Unit = {
    super.inject(language)
    parserProp.add(language, toParserBuilder(language.grammars.root).map(r => r.asInstanceOf[Node]).getWholeInputParser)
  }

  override def description: String = "Parses the input file using a textual grammar."

  override def dependencies: Set[Contract] = Set.empty
}

