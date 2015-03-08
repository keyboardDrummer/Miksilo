package application.graphing.model

import core.particles.Contract

object TransformationVertex {
  implicit def fromTransformation(transformation: Contract) = new TransformationVertex(transformation)
}

case class TransformationVertex(transformation: Contract) {
  override def toString: String = transformation.name
}
