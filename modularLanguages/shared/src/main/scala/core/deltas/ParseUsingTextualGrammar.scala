package core.deltas

import core.LazyLogging
import core.bigrammar.BiGrammarToParser._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.PathRoot
import core.language.node.Node
import core.language.{Compilation, Language}
import core.parsers.editorParsers.{SingleParseResult, SingleResultParser, StopFunction, TimeRatioStopFunction}

case class ParseUsingTextualGrammar(stopFunction: StopFunction = new TimeRatioStopFunction)
  extends Delta with LazyLogging {

  def parseStream[T](compilation: Compilation, parser: SingleResultParser[T], input: String):
    SingleParseResult[T] = {
    parser.parse(input, stopFunction, compilation.metrics)
  }

  override def inject(language: Language): Unit = {
    super.inject(language)
    val parserBuilder = toParserBuilder(LanguageGrammars.grammars.get(language).root).map(r => r.asInstanceOf[Node])

    val phase = Language.getParsePhaseFromParser[Node]((program, uri) => {
      program.startOfUri = Some(uri)
      PathRoot(program)
    }, parserBuilder.getWholeInputParser(), stopFunction)
    language.compilerPhases ::= phase
  }

  override def description: String = "Parses the input file using a textual grammar."

  override def dependencies: Set[Contract] = Set.empty
}

