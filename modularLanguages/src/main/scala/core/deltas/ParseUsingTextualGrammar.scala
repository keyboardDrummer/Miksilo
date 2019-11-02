package core.deltas

import java.io.InputStream

import com.typesafe.scalalogging.LazyLogging
import core.bigrammar.BiGrammarToParser
import core.bigrammar.BiGrammarToParser.{Reader, _}
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.PathRoot
import core.language.node.Node
import core.language.{Compilation, Language}
import core.parsers.core.{Metrics, NoMetrics}
import core.parsers.editorParsers.{SingleParseResult, StopFunction, TimeRatioStopFunction}
import core.parsers.sequences.SingleResultParser
import util.SourceUtils

import scala.io.Source

case class ParseUsingTextualGrammar(stopFunction: StopFunction = new TimeRatioStopFunction)
  extends DeltaWithPhase with LazyLogging {

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val phase = Language.getParsePhaseFromParser[Node, Input](stream => new Reader(Source.fromInputStream(stream, "UTF-8").mkString), (program, uri) => {
      program.startOfUri = Some(uri)
      PathRoot(program)
    }, parserProp.get(compilation), stopFunction)
    phase.action(compilation)
  }

  def parseStream[T](compilation: Compilation, parser: SingleResultParser[T, BiGrammarToParser.Input], input: InputStream): SingleParseResult[T, BiGrammarToParser.Input] = {
    parser.parse(new Reader(SourceUtils.streamToString(input)), stopFunction, compilation.metrics)
  }

  val parserProp = new Property[SingleResultParser[Node, BiGrammarToParser.Input]](null)

  override def inject(language: Language): Unit = {
    super.inject(language)
    parserProp.add(language, toParserBuilder(LanguageGrammars.grammars.get(language).root).map(r => r.asInstanceOf[Node]).getWholeInputParser)
  }

  override def description: String = "Parses the input file using a textual grammar."

  override def dependencies: Set[Contract] = Set.empty
}

