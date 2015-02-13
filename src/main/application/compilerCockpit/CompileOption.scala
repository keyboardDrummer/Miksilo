package application.compilerCockpit

trait CompileOption {
  def perform(cockpit: CompilerCockpit, input: String): String
}
