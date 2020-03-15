package playground.application.compilerCockpit

import java.io.InputStream

import core.deltas.grammars.LanguageGrammars
import core.deltas.{Delta, LanguageFromDeltas, ParseUsingTextualGrammar}
import core.language.Language
import deltas.PrettyPrint
import deltas.json.PrintJson

object FormatOption extends CompileOption {

  val prettyPrint = PrettyPrint(recover = true)
  var language: Language = _

  override def initialize(sandbox: LanguageSandbox): Unit = {
    val startWithPrettyPrint = Seq(PrettyPrint(recover = true)) ++ sandbox.deltas
    language = LanguageFromDeltas(startWithPrettyPrint)
  }

  override def run(sandbox: LanguageSandbox, input: String): TextWithGrammar = {
    val state = language.compileString(input)
    val outputGrammar = prettyPrint.getOutputGrammar(state.language)
    TextWithGrammar(state.output, outputGrammar)
  }

  override def name = "Reformat code"
}

object FormatJsonOption extends CompileOption {

  var language: Language = _
  override def initialize(sandbox: LanguageSandbox): Unit = {
    val startWithPrettyPrint = Delta.spliceAndFilterBottom(Seq(ParseUsingTextualGrammar()), sandbox.deltas, Seq(PrintJson))
    language = LanguageFromDeltas(startWithPrettyPrint)
  }

  override def run(sandbox: LanguageSandbox, input: String): TextWithGrammar = {
    val state = language.compileString(input)
    val outputGrammar = LanguageGrammars.grammars.get(language).root
    TextWithGrammar(state.output, outputGrammar)
  }

  override def name = "Reformat JSON code"
}
