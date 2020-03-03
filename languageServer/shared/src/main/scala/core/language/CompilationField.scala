package core.language

class CachedCompilationField[T <: AnyRef](getDefault: Compilation => T = (_: Compilation) => null.asInstanceOf[T]) {

  private def map(compilation: Compilation): T =
    compilation.cache.state.getOrElseUpdate(this, getDefault(compilation)).asInstanceOf[T]

  def apply(compilation: Compilation): T = map(compilation)

  def update(compilation: Compilation, value: T): Unit = compilation.cache.state.put(this, value)
}

class CompilationField[T <: AnyRef](getDefault: Compilation => T = (_: Compilation) => null.asInstanceOf[T]) {

  private def map(compilation: Compilation): T =
    compilation.data.getOrElseUpdate(this, getDefault(compilation)).asInstanceOf[T]

  def apply(compilation: Compilation): T = map(compilation)

  def update(compilation: Compilation, value: T): Unit = compilation.data.put(this, value)
}
