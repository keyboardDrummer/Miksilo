package miksilo.playground.application.graphing.model

import miksilo.modularLanguages.core.deltas.Contract

object DeltaVertex {
  implicit def fromTransformation(transformation: Contract): DeltaVertex = new DeltaVertex(transformation)
}

case class DeltaVertex(contract: Contract) {
  override def toString: String = contract.name
}
