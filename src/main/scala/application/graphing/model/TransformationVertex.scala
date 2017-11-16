package application.graphing.model

import core.deltas.Contract

object TransformationVertex {
  implicit def fromTransformation(transformation: Contract): TransformationVertex = new TransformationVertex(transformation)
}

case class TransformationVertex(transformation: Contract) {
  override def toString: String = transformation.name
}
