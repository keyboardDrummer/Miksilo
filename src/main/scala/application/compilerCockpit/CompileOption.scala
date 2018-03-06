package application.compilerCockpit

import java.io.InputStream

import core.bigrammar.BiGrammar

case class TextWithGrammar(text: String, grammar: BiGrammar = null)

trait CompileOption {

  def initialize(sandbox: LanguageSandbox): Unit

  def run(sandbox: LanguageSandbox, input: InputStream): TextWithGrammar
}
