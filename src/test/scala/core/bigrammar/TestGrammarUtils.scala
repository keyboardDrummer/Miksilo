package core.bigrammar

import core.bigrammar.printer.BiGrammarToPrinter
import core.grammar.{Grammar, GrammarToParserConverter}
import core.particles._
import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}
import org.scalatest.{FlatSpec, FunSuite, Matchers}
import transformations.javac.JavaCompiler

import scala.util.parsing.input.CharArrayReader

object TestGrammarUtils extends TestGrammarUtils(JavaCompiler.javaCompilerTransformations)

case class TestGrammarUtils(particles: Seq[Particle]) extends FunSuite {

  def parseAndPrintSame(example: String, expectedOption: Option[Any] = None, grammarDocument: BiGrammar): Unit = {
    val documentResult: String = parseAndPrint(example, expectedOption, grammarDocument)
    assertResult(example)(documentResult)
  }

  def parseAndPrint(example: String, expectedOption: Option[Any], grammarDocument: BiGrammar): String = {
    val grammar: Grammar = BiGrammarToGrammar.toGrammar(grammarDocument)

    val packrat: GrammarToParserConverter = new GrammarToParserConverter()
    val packratParser = packrat.convert(grammar)
    val parseResult = packratParser(new CharArrayReader(example.toCharArray))
    assert(parseResult.successful, parseResult.toString)

    val result = parseResult.get

    expectedOption.foreach(expected => assertResult(expected)(result))

    val documentResult = BiGrammarToPrinter.toDocument(result, grammarDocument).renderString()
    documentResult
  }

  def compareInputWithPrint(input: String, expected: Option[Any] = None, grammarTransformer: Any = ProgramGrammar) {
    parseAndPrintSame(input, expected, getGrammarUsingTransformer(grammarTransformer))
  }

  def getPrintResult(value: Any, grammarTransformer: Any = ProgramGrammar): String = {
    val document = getGrammarUsingTransformer(grammarTransformer)
    BiGrammarToPrinter.toDocument(value, document).renderString()
  }

  def getGrammarUsingTransformer(grammarTransformer: Any = ProgramGrammar): Labelled = {
    new CompilerFromParticles(getTransformations(grammarTransformer)).getGrammar
  }

  def getGrammarResult(input: String, grammarTransformer: Any = ProgramGrammar): Any = {
    val compiler = new CompilerFromParticles(getTransformations(grammarTransformer))
    compiler.parse(input)
  }

  def getTransformations(key: Any) = {
    Seq(new SelectorTransformation(key)) ++ particles
  }

  class SelectorTransformation(key: Any) extends ParticleWithGrammar {
    override def transformGrammars(grammars: GrammarCatalogue) {
      grammars.find(ProgramGrammar).inner = grammars.find(key).inner
    }

    override def dependencies: Set[Contract] = Set.empty

    override def description: String = "Sets the program grammar to a specific grammar from the grammar catalogue."
  }

}
