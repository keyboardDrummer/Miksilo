package playground.application.graphing.model.simplifications

import core.deltas.Contract

trait DeltaGroup extends Contract {
  def dependants: Set[Contract]
  def suffix = "Group"
}
