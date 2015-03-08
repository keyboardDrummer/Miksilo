package application.graphing.model.simplifications

import core.particles.Contract

trait TransformationGroup extends Contract {
  def dependants: Set[Contract]
}
