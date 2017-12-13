package core.deltas.node

import core.deltas.path.Path

import scala.collection.mutable
import scala.util.hashing.Hashing

object Node {

  def shapeDebugRepresentation(_shape: Any): String = _shape match {
    case string: String => string
    case anyRef: AnyRef =>
      try
      {
        val shape = anyRef.getClass
        getClassName(shape)
      }
      catch
      {
        case e: java.lang.InternalError => e.toString
      }
    case _ => _shape.toString
  }

  private def getClassName(shape: Class[_]): String = {
    val enclosing = shape.getEnclosingClass
    val addition = if (enclosing == null) "" else getClassName(enclosing) + "."
    addition + getDirectClassName(shape)
  }

  private def getDirectClassName(shape: Class[_]): String = {
    val simpleName: String = shape.getSimpleName
    if (simpleName.last == '$')
      simpleName.dropRight(1)
    else
      simpleName
  }
}

class Node(var shape: NodeShape, entries: (NodeField, Any)*) extends NodeLike {
  type Self = Node

  def shallowClone: Node = {
    val result = new Node(shape)
    result.data ++= data
    result
  }

  def replaceWith(node: Node, keepData: Boolean = false): Unit = {
    shape = node.shape
    if (!keepData)
      data.clear()
    data ++= node.data
  }

  val data: mutable.Map[NodeField, Any] = mutable.Map.empty
  data ++= entries

  def dataView = data.toMap

  def apply(key: NodeField) = data(key)

  def update(key: NodeField, value: Any): Unit = {
    value match //TODO maybe throw this check away.
    {
      case _: Path => throwInsertedWithOriginIntoRegularMetaObject()
      case sequence: Seq[_] => if (sequence.exists(item => item.isInstanceOf[Path]))
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
    s"$className: ${data.map(kv => (Node.shapeDebugRepresentation(kv._1), kv._2))}"
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
}


