package miksilo.modularLanguages.deltas.verilog.preprocessor

import miksilo.modularLanguages.core.bigrammar.BiGrammarToParser._
import miksilo.modularLanguages.core.bigrammar.grammars.StringLiteralGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.{NodePath, NodeSequenceElement}
import miksilo.modularLanguages.core.deltas.{Contract, ParseUsingTextualGrammar, Property}
import miksilo.modularLanguages.core.node.{Node, NodeField, NodeShape}
import miksilo.editorParser.parsers.core.ParseText
import miksilo.editorParser.parsers.editorParsers.{CachingParser, SingleResultParser}
import miksilo.languageServer.core.language.{DiagnosticUtil, Language}
import miksilo.modularLanguages.deltas.FileWithMembersDelta.FileWithMembers

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
    val parseResult = ParseUsingTextualGrammar().parseStream(compilation, parser, input)
    parseResult.resultOption match {
      case Some(success) =>
        val value: FileWithMembers[Node] = success.asInstanceOf[Node]
        value.members.foreach(member => member.startOfUri = Some(filePath.toString()))
        path.asInstanceOf[NodeSequenceElement].replaceWith(value.members)
      case None =>
        val diagnostics = DiagnosticUtil.getDiagnosticsFromParseFailures(fileName, parseResult.errors)
        compilation.addDiagnosticsWithFixes(diagnostics)
    }
  }

  val parserProp = new Property[SingleResultParser[Any]](null)

  override def inject(language: Language): Unit = {
    super.inject(language)
    parserProp.add(language, toParserBuilder(LanguageGrammars.grammars.get(language).root).getWholeInputParser())
  }

  override def dependencies: Set[Contract] = Set(PreprocessorDelta)

  override def shape: NodeShape = Shape

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val grammar = "include" ~~> StringLiteralGrammar.as(FileName).asNode(Shape)
    find(PreprocessorDelta.BodyGrammar).addAlternative(grammar)
  }

  object Shape extends NodeShape
  object FileName extends NodeField
}
