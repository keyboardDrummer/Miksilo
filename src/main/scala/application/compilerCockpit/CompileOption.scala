package application.compilerCockpit

import java.io.InputStream

import core.grammar.Grammar

case class TextWithGrammar(text: String, grammar: Grammar = null)

trait CompileOption {
  def perform(cockpit: CompilerCockpit, input: InputStream): TextWithGrammar
}
