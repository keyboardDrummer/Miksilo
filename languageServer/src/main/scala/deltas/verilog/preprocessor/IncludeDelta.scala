package deltas.verilog.preprocessor

import core.bigrammar.BiGrammarToParser
import core.bigrammar.grammars.{ParseWhiteSpace, StringLiteral}
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.{ChildPath, NodePath}
import core.deltas.{Contract, DiagnosticUtil, ParseUsingTextualGrammar, Property}
import core.language.Language
import core.language.node.{NodeField, NodeShape}

object IncludeDelta extends DirectiveDelta {
  override def description: String = "Adds the `include <filename> directive"

  override def apply(preprocessor: Preprocessor, path: NodePath): Unit = {
    val fileName = path.current(FileName).asInstanceOf[String]
    val compilation = preprocessor.compilation
    val input = preprocessor.compilation.fileSystem.getFile(fileName)

    val parser: BiGrammarToParser.PackratParser[Any] = parserProp.get(compilation)
    val parseResult = ParseUsingTextualGrammar.parseStream(parser, input)
    if (parseResult.successful)
      path.asInstanceOf[ChildPath].replaceWith(parseResult.get)
    else
      compilation.diagnostics ++= List(DiagnosticUtil.getDiagnosticFromParseException(parseResult.toString))
  }

  val parserProp = new Property[BiGrammarToParser.PackratParser[Any]](null)

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
