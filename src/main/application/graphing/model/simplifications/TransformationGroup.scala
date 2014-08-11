package application.graphing.model.simplifications

import core.transformation.Contract

trait TransformationGroup extends Contract {
  def dependants: Set[Contract]
}
