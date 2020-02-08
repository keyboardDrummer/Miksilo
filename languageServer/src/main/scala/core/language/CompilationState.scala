package core.language

class CompilationState[T](default: Compilation => T = null) {

  private def map(compilation: Compilation): T =
    compilation.state.getOrElseUpdate(this, default).asInstanceOf[T]

  def apply(compilation: Compilation): T = map(compilation)

  def update(compilation: Compilation, value: T): Unit = compilation.state.put(this, value)
}
