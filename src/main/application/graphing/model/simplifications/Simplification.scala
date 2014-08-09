package application.graphing.model.simplifications

import core.transformation.Contract

trait Simplification extends Contract {
  def dependants: Set[Contract]
}
