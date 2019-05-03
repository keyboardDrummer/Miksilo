package core.deltas

import core.bigrammar.BiGrammarToParser._
import core.language.node.Node
import core.language.{Compilation, Language}
import core.smarts.FileDiagnostic
import util.SourceUtils

import scala.tools.nsc.interpreter.InputStream

object ParseUsingTextualGrammar extends DeltaWithPhase {

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val parser = parserProp.get(compilation)

    val uri = compilation.rootFile.get
    val inputStream = compilation.fileSystem.getFile(uri)
    val parseResult: ParseWholeResult[Node] = parseStream(parser, inputStream)
    parseResult.resultOption.foreach(program => {
      compilation.program = program
      compilation.program.startOfUri = Some(uri)
    })
    if (compilation.program == null) {
      compilation.stopped = true
    }
    if (!parseResult.successful) {
      val diagnostics = DiagnosticUtil.getDiagnosticFromParseFailure(parseResult.errors)
      compilation.diagnostics ++= diagnostics.map(d => FileDiagnostic(uri, d))
    }
  }

  def parseStream[T](parser: Self[T], input: InputStream): ParseWholeResult[T] = {
    val reader = new Reader(SourceUtils.streamToString(input))
    parser.parseWholeInput(reader)
  }

  val parserProp = new Property[Self[Node]](null)

  override def inject(language: Language): Unit = {
    super.inject(language)
    parserProp.add(language, toParser(language.grammars.root).map(r => r.asInstanceOf[Node]))
  }

  override def description: String = "Parses the input file using a textual grammar."

  override def dependencies: Set[Contract] = Set.empty
}

