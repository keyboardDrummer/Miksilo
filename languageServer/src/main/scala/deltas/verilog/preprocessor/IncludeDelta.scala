package deltas.verilog.preprocessor

import core.bigrammar.BiGrammarToParser._
import core.bigrammar.grammars.{ParseWhiteSpace, StringLiteral}
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.{NodePath, NodeSequenceElement}
import core.deltas.{Contract, DiagnosticUtil, ParseUsingTextualGrammar, Property}
import core.language.Language
import core.language.node.{Node, NodeField, NodeShape}
import core.smarts.FileDiagnostic
import deltas.verilog.VerilogFileDelta.VerilogFile

import scala.reflect.io.Path

object IncludeDelta extends DirectiveDelta {
  override def description: String = "Adds the `include <filename> directive"

  override def apply(preprocessor: Preprocessor, path: NodePath): Unit = {
    val fileName = path.current(FileName).asInstanceOf[String]
    val compilation = preprocessor.compilation
    val rootDirectory = Path(preprocessor.compilation.rootFile.get).parent
    val filePath: Path = rootDirectory / Path.apply(fileName)
    val input = preprocessor.compilation.fileSystem.getFile(filePath.toString())

    val parser = parserProp.get(compilation)
    val parseResult = ParseUsingTextualGrammar.parseStream(parser, input)
    parseResult.resultOption match {
      case Some(result) =>
        val value: VerilogFile[Node] = result.asInstanceOf[Node]
        value.members.foreach(member => member.startOfUri = Some(filePath.toString()))
        path.asInstanceOf[NodeSequenceElement].replaceWith(value.members)
      case None =>
        val diagnostics = DiagnosticUtil.getDiagnosticFromParseFailure(parseResult.errors)
        compilation.diagnostics ++= diagnostics.map(d => FileDiagnostic(filePath.toString(), d))
    }
  }

  val parserProp = new Property[Input => ParseWholeResult[Any]](null)

  override def inject(language: Language): Unit = {
    super.inject(language)
    parserProp.add(language, toParserBuilder(language.grammars.root).getWholeInputParser())
  }

  override def dependencies: Set[Contract] = Set(PreprocessorDelta)

  override def shape: NodeShape = Shape

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val grammar = "include" ~~> StringLiteral.as(FileName).asNode(Shape)
    find(PreprocessorDelta.BodyGrammar).addAlternative(grammar)
  }

  object Shape extends NodeShape
  object FileName extends NodeField
}
