package application.compilerCockpit

import java.io.InputStream

import core.bigrammar.BiGrammar

case class TextWithGrammar(text: String, grammar: BiGrammar = null)

trait CompileOption {
  def perform(cockpit: LanguageSandbox, input: InputStream): TextWithGrammar
}
