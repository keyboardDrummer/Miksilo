package core.grammarDocument

import core.grammar.{Grammar, ToPackrat}
import core.transformation._
import core.transformation.grammars.{GrammarCatalogue, ProgramGrammar}
import core.transformation.sillyCodePieces.GrammarTransformation
import org.junit.Assert
import transformations.javac.JavaCompiler

import scala.util.parsing.input.CharArrayReader

object TestGrammarUtils {

  def parseAndPrint(example: String, expectedOption: Option[Any] = None, grammarDocument: Labelled) {
    val grammar: Grammar = GrammarDocumentToGrammar.toGrammar(grammarDocument)

    val packrat: ToPackrat = new ToPackrat()
    val packratParser = packrat.convert(grammar)
    val parseResult = packratParser(new CharArrayReader(example.toCharArray))
    if (parseResult.isEmpty)
      Assert.fail(parseResult.toString)

    val result = parseResult.get

    expectedOption.foreach(expected =>
      Assert.assertEquals(expected, result))

    val documentResult = PrintValueUsingGrammarDocument.toDocument(result, grammarDocument).renderString()
    Assert.assertEquals(example, documentResult)
  }

  def compareInputWithPrint(input: String, expected: Option[Any] = None, grammarTransformer: Any = ProgramGrammar) {
    parseAndPrint(input, expected, getGrammarUsingTransformer(grammarTransformer))
  }

  def getPrintResult(value: Any, grammarTransformer: Any = ProgramGrammar): String = {
    val document = getGrammarUsingTransformer(grammarTransformer)
    PrintValueUsingGrammarDocument.toDocument(value, document).renderString()
  }

  def getGrammarUsingTransformer(grammarTransformer: Any): Labelled = {
    GrammarDocumentUtil.getGrammarFromTransformations(getTransformations(grammarTransformer))
  }

  def getGrammarResult(input: String, grammarTransformer: Any = ProgramGrammar): Any = {
    val manager: TransformationsToPackrat = new TransformationsToPackrat()
    val parser = manager.buildParser(getTransformations(grammarTransformer))
    val parseResult = parser(input)
    if (parseResult.isEmpty)
      Assert.fail(parseResult.toString)

    val result = parseResult.get
    Assert.assertTrue(result.toString, parseResult.next.atEnd)
    result
  }

  def getTransformations(key: Any): Seq[GrammarTransformation] = {
    JavaCompiler.javaCompilerTransformations.reverse.collect({ case x: GrammarTransformation => x}) ++
      Seq(new SelectorTransformation(key))
  }

  class SelectorTransformation(key: Any) extends GrammarTransformation {
    override def transformGrammars(grammars: GrammarCatalogue) {
      grammars.find(ProgramGrammar).inner = grammars.find(key).inner
    }

    override def dependencies: Set[Contract] = Set.empty
  }

}
