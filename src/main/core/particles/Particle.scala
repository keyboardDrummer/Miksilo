package core.particles



trait Particle extends Contract {

  def inject(state: CompilationState) = {

  }

  final def dependencies2: Set[Particle] = dependencies.collect({case x: Particle => x})

  def description: String
}
