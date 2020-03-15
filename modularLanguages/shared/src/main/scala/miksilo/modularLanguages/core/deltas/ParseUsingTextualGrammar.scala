package miksilo.modularLanguages.core.deltas

import miksilo.editorParser.LazyLogging
import miksilo.modularLanguages.core.bigrammar.BiGrammarToParser._
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.PathRoot
import miksilo.modularLanguages.core.node.Node
import miksilo.languageServer.core.language.{Compilation, Language}
import core.parsers.SourceElement
import miksilo.editorParser.parsers.editorParsers.{SingleParseResult, SingleResultParser, StopFunction, TimeRatioStopFunction}
import miksilo.languageServer.server.SourcePath

case class ParseUsingTextualGrammar(stopFunction: StopFunction = new TimeRatioStopFunction, indentationSensitive: Boolean = false)
  extends Delta with LazyLogging {

  def parseStream[T](compilation: Compilation, parser: SingleResultParser[T], input: String):
    SingleParseResult[T] = {
    parser.parse(input, stopFunction, compilation.metrics)
  }

  override def inject(language: Language): Unit = {
    super.inject(language)
    val parserBuilder = toParserBuilder(LanguageGrammars.grammars.get(language).root).map(r => r.asInstanceOf[Node])

    val parser = parserBuilder.getWholeInputParser()
    val phase = Language.getCachingParsePhase[Node](toSourceElement, parser, stopFunction, indentationSensitive)
    language.compilerPhases ::= phase
  }

  private def toSourceElement(program: Node, uri: String): SourcePath = {
    program.startOfUri = Some(uri)
    PathRoot(program)
  }

  override def description: String = "Parses the input file using a textual grammar."

  override def dependencies: Set[Contract] = Set.empty
}

