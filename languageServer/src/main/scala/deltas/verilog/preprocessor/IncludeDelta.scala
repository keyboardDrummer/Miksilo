package deltas.verilog.preprocessor

import core.bigrammar.BiGrammarToParser
import core.bigrammar.grammars.{ParseWhiteSpace, StringLiteral}
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.{NodePath, SequenceElement}
import core.deltas.{Contract, DiagnosticUtil, ParseUsingTextualGrammar, Property}
import core.language.Language
import core.language.node.{Node, NodeField, NodeShape}
import core.parsers.{ParseFailure, ParseSuccess}
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

    val parser: BiGrammarToParser.Parser[Any] = parserProp.get(compilation)
    val parseResult = ParseUsingTextualGrammar.parseStream(parser, input)
    parseResult match {
      case success: ParseSuccess[_, _] =>
        val value: VerilogFile[Node] = success.result.asInstanceOf[Node]
        value.members.foreach(member => member.startOfUri = Some(filePath.toString()))
        path.asInstanceOf[SequenceElement].replaceWith(value.members)
      case failure: ParseFailure[_, _] =>
        val diagnostic = DiagnosticUtil.getDiagnosticFromParseException(failure.message)
        compilation.diagnostics ++= List(FileDiagnostic(filePath.toString(), diagnostic))
    }
  }

  val parserProp = new Property[BiGrammarToParser.Parser[Any]](null)

  override def inject(language: Language): Unit = {
    super.inject(language)
    parserProp.add(language, BiGrammarToParser.toParser(language.grammars.root))
  }

  override def dependencies: Set[Contract] = Set(PreprocessorDelta)

  override def shape: NodeShape = Shape

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import core.bigrammar.BasicSequenceCombinators._

    val grammar = "include" ~ ParseWhiteSpace ~~> StringLiteral.as(FileName).asNode(Shape)
    grammars.find(PreprocessorDelta.BodyGrammar).addAlternative(grammar)
  }

  object Shape extends NodeShape
  object FileName extends NodeField
}
