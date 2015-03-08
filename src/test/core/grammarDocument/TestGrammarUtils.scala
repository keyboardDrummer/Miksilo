package core.grammarDocument

import core.grammar.{Grammar, ToPackrat}
import core.transformation._
import core.transformation.grammars.{GrammarCatalogue, ProgramGrammar}
import core.transformation.sillyCodePieces.GrammarTransformation
import org.junit.Assert
import transformations.javac.JavaCompiler

import scala.util.parsing.input.CharArrayReader

object TestGrammarUtils {

  def parseAndPrint(example: String, expectedOption: Option[Any] = None, grammarDocument: BiGrammar) {
    val grammar: Grammar = BiGrammarToGrammar.toGrammar(grammarDocument)

    val packrat: ToPackrat = new ToPackrat()
    val packratParser = packrat.convert(grammar)
    val parseResult = packratParser(new CharArrayReader(example.toCharArray))
    if (parseResult.isEmpty)
      Assert.fail(parseResult.toString)

    val result = parseResult.get

    expectedOption.foreach(expected =>
      Assert.assertEquals(expected, result))

    val documentResult = BiGrammarToDocument.toDocument(result, grammarDocument).renderString()
    Assert.assertEquals(example, documentResult)
  }

  def compareInputWithPrint(input: String, expected: Option[Any] = None, grammarTransformer: Any = ProgramGrammar) {
    parseAndPrint(input, expected, getGrammarUsingTransformer(grammarTransformer))
  }

  def getPrintResult(value: Any, grammarTransformer: Any = ProgramGrammar): String = {
    val document = getGrammarUsingTransformer(grammarTransformer)
    BiGrammarToDocument.toDocument(value, document).renderString()
  }

  def getGrammarUsingTransformer(grammarTransformer: Any): Labelled = {
    new CompilerFromParticles(getTransformations(grammarTransformer)).getGrammar
  }

  def getGrammarResult(input: String, grammarTransformer: Any = ProgramGrammar): Any = {
    val compiler = new CompilerFromParticles(getTransformations(grammarTransformer))
    compiler.parse(input)
  }

  def getTransformations(key: Any) = {
    Seq(new SelectorTransformation(key)) ++ JavaCompiler.javaCompilerTransformations
  }

  class SelectorTransformation(key: Any) extends GrammarTransformation {
    override def transformGrammars(grammars: GrammarCatalogue) {
      grammars.find(ProgramGrammar).inner = grammars.find(key).inner
    }

    override def dependencies: Set[Contract] = Set.empty

    override def description: String = "Sets the program grammar to a specific grammar from the grammar catalogue."
  }

}
