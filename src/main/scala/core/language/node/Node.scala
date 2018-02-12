package core.language.node

import core.deltas.path.NodePath
import core.language.SourceElement

import scala.collection.mutable
import scala.util.hashing.Hashing

class Node(var shape: NodeShape, entries: (NodeField, Any)*)
  extends NodeLike {
  type Self = Node

  def shallowClone: Node = {
    val result = new Node(shape)
    result.data ++= data
    result
  }

  override def asNode: Node = this
  override def asPath: Option[NodePath] = None

  def replaceWith(node: Node, keepData: Boolean = false): Unit = {
    shape = node.shape
    if (!keepData)
      data.clear()
    data ++= node.data
  }

  val sources: mutable.Map[NodeField, SourceRange] = mutable.Map.empty
  val data: mutable.Map[NodeField, Any] = mutable.Map.empty
  data ++= entries

  def dataView: Map[NodeField, Any] = data.toMap

  def apply(key: NodeField) = data(key)

  def update(key: NodeField, value: Any): Unit = {
    value match //TODO maybe throw this check away.
    {
      case _: NodePath => throwInsertedWithOriginIntoRegularMetaObject()
      case sequence: Seq[_] => if (sequence.exists(item => item.isInstanceOf[NodePath]))
        throwInsertedWithOriginIntoRegularMetaObject()
      case _ =>
    }
    data.put(key, value)
  }

  def throwInsertedWithOriginIntoRegularMetaObject(): Unit = {
    throw new scala.UnsupportedOperationException("Don't insert a Path into a Node.")
  }

  override def toString: String = {
    val className = shape.toString
    if (data.isEmpty)
      return className
    s"$className: ${data.map(kv => (kv._1.debugRepresentation, kv._2))}"
  }

  override def equals(other: Any): Boolean = other match {
    case that: Node =>
      val dataEquals: Boolean = data == that.data
      (that canEqual this) &&
        dataEquals &&
        shape == that.shape
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Node]

  override def hashCode(): Int = {
    val state = Seq(data, shape)
    Hashing.default.hash(state)
  }

  override def get(key: NodeField): Option[Any] = data.get(key)

  override def getLocation(field: NodeField): SourceElement = FieldLocation(this, field)
}


