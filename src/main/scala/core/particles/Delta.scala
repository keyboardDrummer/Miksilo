package core.particles

trait Delta extends Contract {

  def inject(state: CompilationState) = {  }

  final def dependencies2: Set[Delta] = dependencies.collect({case x: Delta => x})

  def description: String
}
