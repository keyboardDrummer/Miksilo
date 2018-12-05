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
    val parser = parserProp.get(compilation)

    val uri = compilation.rootFile.get
    val inputStream = compilation.fileSystem.getFile(uri)
    val parseResult: BiGrammarToParser.ParseResult[Node] = parseStream(parser, inputStream)
    parseResult.getPartial.foreach(program => {
      compilation.program = program
      compilation.program.startOfUri = Some(uri)
    })
    parseResult match {
      case failure: BiGrammarToParser.ParseFailure[Node] =>
        compilation.diagnostics ++= List(FileDiagnostic(uri, DiagnosticUtil.getDiagnosticFromParseFailure(failure)))
      case _ =>
    }
  }

  def parseStream[T](parser: BiGrammarToParser.EditorParser[T], input: InputStream): BiGrammarToParser.ParseResult[T] = {
    val reader = new StringReader(SourceUtils.streamToString(input))
    parser.parseWholeInput(reader)
  }

  val parserProp = new Property[BiGrammarToParser.EditorParser[Node]](null)

  override def inject(language: Language): Unit = {
    super.inject(language)
    parserProp.add(language, BiGrammarToParser.toParser(language.grammars.root).map(r => r.asInstanceOf[Node]))
  }

  override def description: String = "Parses the input file using a textual grammar."

  override def dependencies: Set[Contract] = Set.empty
}

