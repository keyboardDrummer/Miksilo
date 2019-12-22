package core.bigrammar

import core.bigrammar.printer.BiGrammarToPrinter
import core.deltas.Delta
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.PathRoot
import core.language.node.GrammarKey
import deltas.ClearPhases
import deltas.javac.JavaToByteCodeLanguage
import org.scalatest.funsuite.AnyFunSuite
import util.TestLanguageBuilder

case class StringKey(value: String) extends GrammarKey {
  override lazy val toString: String = value
}

object TestLanguageGrammarUtils extends TestLanguageGrammarUtils(JavaToByteCodeLanguage.javaCompilerDeltas)

case class TestLanguageGrammarUtils(deltas: Seq[Delta]) extends AnyFunSuite {

  def compareInputWithPrint(input: String, expected: Option[Any] = None, grammarTransformer: GrammarKey = null): Unit = {
    val grammar = getGrammarUsingTransformer(grammarTransformer)
    TestGrammarUtils.parseAndPrintSame(input, expected, grammar)
  }

  def getPrintResult(value: Any, grammarTransformer: GrammarKey = null): String = {
    val document = getGrammarUsingTransformer(grammarTransformer)
    BiGrammarToPrinter.toDocument(value, document).renderString()
  }

  def getGrammarUsingTransformer(grammarTransformer: GrammarKey = null): BiGrammar = {
    val language = TestLanguageBuilder.buildWithParser(getDeltas(grammarTransformer)).language
    LanguageGrammars.grammars.get(language).root
  }

  def parse(input: String, grammarTransformer: GrammarKey = null): Any = {
    val compiler = TestLanguageBuilder.buildWithParser(Seq(ClearPhases) ++ getDeltas(grammarTransformer))
    val result = compiler.compileString(input).program.asInstanceOf[PathRoot].current
    result.startOfUri = None
    result
  }

  def getDeltas(key: GrammarKey): Seq[Delta] = {
    Seq(new SelectGrammar(key)) ++ deltas
  }

}


