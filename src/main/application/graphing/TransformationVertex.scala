package application.graphing

import core.transformation.ProgramTransformation

object TransformationVertex
{
  implicit def fromTransformation(transformation: ProgramTransformation) = new TransformationVertex(transformation)
}

case class TransformationVertex(transformation: ProgramTransformation)
{
  override def toString: String = transformation.getClass.getSimpleName.dropRight(1)
}
