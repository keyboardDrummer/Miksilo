package application.graphing

import core.transformation.Contract

object TransformationVertex
{
  implicit def fromTransformation(transformation: Contract) = new TransformationVertex(transformation)
}

case class TransformationVertex(transformation: Contract) {
  override def toString: String = transformation.name
}
