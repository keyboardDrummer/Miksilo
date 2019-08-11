package core.deltas

import core.bigrammar.BiGrammarToParser._
import core.language.node.Node
import core.language.{Compilation, Language}
import core.parsers.editorParsers.{StopFunction, TimeRatioStopFunction}
import util.SourceUtils

import scala.tools.nsc.interpreter.InputStream

case class ParseUsingTextualGrammar(stopFunction: StopFunction = new TimeRatioStopFunction) extends DeltaWithPhase {

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val parser = parserProp.get(compilation)

    val uri = compilation.rootFile.get
    val inputStream = compilation.fileSystem.getFile(uri)
    val parseResult: SingleParseResult[Node] = parseStream(parser, inputStream)
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

  def parseStream[T](parser: SingleResultParser[T], input: InputStream): SingleParseResult[T] = {
    parser.parse(new Reader(SourceUtils.streamToString(input)), stopFunction)
  }

  val parserProp = new Property[SingleResultParser[Node]](null)

  override def inject(language: Language): Unit = {
    super.inject(language)
    parserProp.add(language, toParserBuilder(language.grammars.root).map(r => r.asInstanceOf[Node]).getWholeInputParser)
  }

  override def description: String = "Parses the input file using a textual grammar."

  override def dependencies: Set[Contract] = Set.empty
}

