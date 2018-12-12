package core.deltas

import core.bigrammar.BiGrammarToParser._
import core.language.node.Node
import core.language.{Compilation, Language}
import core.parsers.strings.StringReader
import core.smarts.FileDiagnostic
import util.SourceUtils

import scala.tools.nsc.interpreter.InputStream

object ParseUsingTextualGrammar extends DeltaWithPhase {

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val parser = parserProp.get(compilation)

    val uri = compilation.rootFile.get
    val inputStream = compilation.fileSystem.getFile(uri)
    val parseResult: ParseResult[Node] = parseStream(parser, inputStream)
    parseResult.getPartial.foreach(program => {
      compilation.program = program
      compilation.program.startOfUri = Some(uri)
    })
    parseResult match {
      case failure: ParseFailure[_] =>
        compilation.diagnostics ++= List(FileDiagnostic(uri, DiagnosticUtil.getDiagnosticFromParseFailure(failure)))
      case _ =>
    }
  }

  def parseStream[T](parser: EditorParser[T], input: InputStream): ParseResult[T] = {
    val reader = new StringReader(SourceUtils.streamToString(input))
    parser.parseWholeInput(reader)
  }

  val parserProp = new Property[EditorParser[Node]](null)

  override def inject(language: Language): Unit = {
    super.inject(language)
    parserProp.add(language, toParser(language.grammars.root).map(r => r.asInstanceOf[Node]))
  }

  override def description: String = "Parses the input file using a textual grammar."

  override def dependencies: Set[Contract] = Set.empty
}

