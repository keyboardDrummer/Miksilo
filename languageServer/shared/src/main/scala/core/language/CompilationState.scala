package core.language

class CompilationState[T <: AnyRef](getDefault: Compilation => T = (_: Compilation) => null.asInstanceOf[T]) {

  private def map(compilation: Compilation): T =
    compilation.state.getOrElseUpdate(this, getDefault(compilation)).asInstanceOf[T]

  def apply(compilation: Compilation): T = map(compilation)

  def update(compilation: Compilation, value: T): Unit = compilation.state.put(this, value)
}
