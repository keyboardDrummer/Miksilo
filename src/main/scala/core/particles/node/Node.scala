package core.particles.node

import core.particles.path.Path

import scala.collection.mutable

object Node {

  def classDebugRepresentation(_clazz: Any): String = _clazz match {
    case string: String => string
    case anyRef: AnyRef =>
      try
      {
        val simpleName: String = anyRef.getClass.getSimpleName
        if (simpleName.last == '$')
          return simpleName.dropRight(1)
        simpleName
      }
      catch
      {
        case e: java.lang.InternalError => "internalError"
      }
    case _ => _clazz.toString
  }
}

class Node(var clazz: AnyRef, entries: (Any, Any)*) extends NodeLike { 
  type Self = Node

  def shallowClone: Node = {
    val result = new Node(clazz)
    result.data ++= data
    result
  }

  def replaceWith(node: Node, keepData: Boolean = false): Unit = {
    clazz = node.clazz
    if (!keepData)
      data.clear()
    data ++= node.data
  }

  val data: mutable.Map[Any, Any] = mutable.Map.empty
  data ++= entries

  def dataView = data.toMap

  def apply(key: Any) = data(key)

  def update(key: Any, value: Any): Unit = {
    value match //TODO maybe throw this check away.
    {
      case wrong: Path => throwInsertedWithOriginIntoRegularMetaObject()
      case sequence: Seq[_] => if (sequence.exists(item => item.isInstanceOf[Path]))
        throwInsertedWithOriginIntoRegularMetaObject()
      case _ =>
    }

    data.put(key, value)
  }

  def throwInsertedWithOriginIntoRegularMetaObject(): Unit = {
    throw new scala.UnsupportedOperationException("Don't insert a Origin into a regular MetaObject.")
  }

  override def toString: String = {
    val className = Node.classDebugRepresentation(clazz)
    if (data.isEmpty)
      return className
    s"$className: ${data.map(kv => (Node.classDebugRepresentation(kv._1), kv._2))}"
  }

  override def equals(other: Any): Boolean = other match {
    case that: Node =>
      val dataEquals: Boolean = data == that.data
      (that canEqual this) &&
        dataEquals &&
        clazz == that.clazz
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Node]

  override def hashCode(): Int = {
    val state = Seq(data, clazz)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def get(key: Any): Option[Any] = data.get(key)
}


