package core.language.node

import core.deltas.path.{ChildPath, NodePath}

import scala.collection.mutable

class TypedChildField[T](name: String) extends NodeField {
  def get(path: ChildPath): Option[T] = getFieldData(path).get(this).asInstanceOf[Option[T]]
  def apply(path: ChildPath): T = getFieldData(path)(this).asInstanceOf[T]

  private def getFieldData(path: ChildPath): mutable.Map[NodeField, Any] = {
    path match {
      case nodePath: NodePath => nodePath.data
      case _ => path.parent.current.fieldData.getOrElseUpdate(path.field, mutable.Map.empty)
    }
  }

  def update(path: ChildPath, value: T): Unit = getFieldData(path)(this) = value

  override lazy val toString = name
}
