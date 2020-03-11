package core.deltas

import core.LazyLogging
import core.bigrammar.BiGrammarToParser._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.PathRoot
import core.language.node.Node
import core.language.{Compilation, Language, SourceElement}
import core.parsers.editorParsers.{SingleParseResult, SingleResultParser, StopFunction, TimeRatioStopFunction}

case class ParseUsingTextualGrammar(stopFunction: StopFunction = new TimeRatioStopFunction, useCaching: Boolean = true)
  extends Delta with LazyLogging {

  def parseStream[T](compilation: Compilation, parser: SingleResultParser[T], input: String):
    SingleParseResult[T] = {
    parser.parse(input, stopFunction, compilation.metrics)
  }

  override def inject(language: Language): Unit = {
    super.inject(language)
    val parserBuilder = toParserBuilder(LanguageGrammars.grammars.get(language).root).map(r => r.asInstanceOf[Node])

    val parser = parserBuilder.getWholeInputParser()
    val phase =
      if (useCaching)
        Language.getCachingParsePhase[Node](toSourceElement, parser, stopFunction)
      else
        Language.getParsePhase[Node](toSourceElement, parser, stopFunction)
    language.compilerPhases ::= phase
  }

  private def toSourceElement(program: Node, uri: String): SourceElement = {
    program.startOfUri = Some(uri)
    PathRoot(program)
  }

  override def description: String = "Parses the input file using a textual grammar."

  override def dependencies: Set[Contract] = Set.empty
}

