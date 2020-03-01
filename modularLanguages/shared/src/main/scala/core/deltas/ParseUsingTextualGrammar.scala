package core.deltas

import core.LazyLogging
import core.bigrammar.BiGrammarToParser
import core.bigrammar.BiGrammarToParser.{Reader, _}
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.PathRoot
import core.language.node.Node
import core.language.{Compilation, CompilationState, Language, Phase}
import core.parsers.editorParsers.{SingleParseResult, StopFunction, TimeRatioStopFunction}
import core.parsers.sequences.SingleResultParser

case class ParseUsingTextualGrammar(stopFunction: StopFunction = new TimeRatioStopFunction)
  extends Delta with LazyLogging {

  def parseStream[T](compilation: Compilation, parser: SingleResultParser[T, BiGrammarToParser.Input], input: String):
    SingleParseResult[T, BiGrammarToParser.Input] = {
    parser.parse(input, stopFunction, compilation.metrics)
  }

  override def inject(language: Language): Unit = {
    super.inject(language)
    val parserBuilder = ParseWholeInput(toParserBuilder(LanguageGrammars.grammars.get(language).root).map(r => r.asInstanceOf[Node]))

    val phase = Language.getParsePhaseFromParser[Node](BiGrammarToParser)((program, uri) => {
      program.startOfUri = Some(uri)
      PathRoot(program)
    }, parserBuilder, stopFunction)
    language.compilerPhases ::= phase
  }

  override def description: String = "Parses the input file using a textual grammar."

  override def dependencies: Set[Contract] = Set.empty
}

