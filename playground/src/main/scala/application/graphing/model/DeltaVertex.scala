package application.graphing.model

import core.deltas.Contract

object DeltaVertex {
  implicit def fromTransformation(transformation: Contract): DeltaVertex = new DeltaVertex(transformation)
}

case class DeltaVertex(contract: Contract) {
  override def toString: String = contract.name
}
