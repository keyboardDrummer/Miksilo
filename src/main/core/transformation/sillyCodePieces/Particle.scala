package core.transformation.sillyCodePieces

import core.transformation.{Contract, TransformationState}

trait Particle extends Contract {

  def inject(state: TransformationState) = {

  }

  final def dependencies2: Set[Particle] = dependencies.collect({case x: Particle => x})

  def description: String
}
