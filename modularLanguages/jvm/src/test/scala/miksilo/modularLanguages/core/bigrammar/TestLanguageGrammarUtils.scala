package miksilo.modularLanguages.core.bigrammar

import miksilo.modularLanguages.core.bigrammar.printer.BiGrammarToPrinter
import miksilo.modularLanguages.core.deltas.Delta
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.PathRoot
import miksilo.modularLanguages.core.node.GrammarKey
import miksilo.modularLanguages.deltas.ClearPhases
import miksilo.modularLanguages.deltas.javac.JavaToByteCodeLanguage
import miksilo.modularLanguages.util.TestLanguageBuilder
import org.scalatest.funsuite.AnyFunSuite

object TestLanguageGrammarUtils extends TestLanguageGrammarUtils(JavaToByteCodeLanguage.javaCompilerDeltas)

class TestLanguageGrammarUtils(deltas: Seq[Delta]) {

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
    val compilation = compiler.compileString(input)
    val result = compilation.program.asInstanceOf[PathRoot].current
    result.startOfUri = None
    result
  }

  def getDeltas(key: GrammarKey): Seq[Delta] = {
    Seq(new SelectGrammar(key)) ++ deltas
  }

}


