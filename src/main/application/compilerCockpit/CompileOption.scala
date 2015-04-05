package application.compilerCockpit

import java.io.InputStream

trait CompileOption {
  def perform(cockpit: CompilerCockpit, input: InputStream): String
}
