package miksilo.playground.application.compilerCockpit

import java.io.InputStream



trait CompileOption {

  def name: String

  override def toString: String = name

  def initialize(sandbox: LanguageSandbox): Unit

  def run(sandbox: LanguageSandbox, input: String): TextWithGrammar
}
