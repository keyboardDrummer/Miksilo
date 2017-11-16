package application.graphing.model.simplifications

import core.deltas.Contract

trait TransformationGroup extends Contract {
  def dependants: Set[Contract]
}
