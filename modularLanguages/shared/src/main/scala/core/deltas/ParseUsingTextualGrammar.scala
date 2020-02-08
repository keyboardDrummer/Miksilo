package core.deltas

import core.bigrammar.BiGrammarToParser
import core.bigrammar.BiGrammarToParser.{Reader, _}
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.PathRoot
import core.language.node.Node
import core.language.{Compilation, CompilationState, Language}
import core.parsers.editorParsers.{SingleParseResult, StopFunction, TimeRatioStopFunction}
import core.parsers.sequences.SingleResultParser
import jsonRpc.LazyLogging

case class ParseUsingTextualGrammar(stopFunction: StopFunction = new TimeRatioStopFunction)
  extends DeltaWithPhase with LazyLogging {

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val phase = Language.getParsePhaseFromParser[Node](BiGrammarToParser)(input => new Reader(input), (program, uri) => {
      program.startOfUri = Some(uri)
      PathRoot(program)
    }, parserProp.get(compilation), stopFunction)
    phase.action(compilation)
  }

  def parseStream[T](compilation: Compilation, parser: SingleResultParser[T, BiGrammarToParser.Input], input: String):
    SingleParseResult[T, BiGrammarToParser.Input] = {
    parser.parse(new Reader(input), stopFunction, compilation.metrics)
  }

  val parserProp = new Property[Parser[Node]](null)

  override def inject(language: Language): Unit = {
    super.inject(language)
    parserProp.add(language, ParseWholeInput(toParserBuilder(LanguageGrammars.grammars.get(language).root).map(r => r.asInstanceOf[Node])))
  }

  override def description: String = "Parses the input file using a textual grammar."

  override def dependencies: Set[Contract] = Set.empty
}

