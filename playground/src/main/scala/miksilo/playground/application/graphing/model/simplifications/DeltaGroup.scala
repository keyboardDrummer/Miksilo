package miksilo.playground.application.graphing.model.simplifications

import miksilo.modularLanguages.core.deltas.Contract

trait DeltaGroup extends Contract {
  def dependants: Set[Contract]
  def suffix = "Group"
}
