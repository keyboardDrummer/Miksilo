package core.language.node

import core.deltas.path.NodePath
import core.language.node.Node._
import core.parsers.editorParsers.{OffsetNodeRange, OffsetRange}

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

  def replaceData(node: Node, keepData: Boolean = false): Unit = {
    shape = node.shape
    if (!keepData) {
      data.clear()
      sources.clear()
      fieldData.clear()
    }
    fieldData ++= node.fieldData
    data ++= node.data
  }

  def removeField(field: NodeField): Unit = {
    sources.remove(field)
    data.remove(field)
    fieldData.remove(field)
  }

  var startOfUri: Option[String] = None
  val fieldData: mutable.Map[NodeField, mutable.Map[NodeField, Any]] = mutable.Map.empty
  val sources: mutable.Map[NodeField, OffsetNodeRange] = mutable.Map.empty
  val data: mutable.Map[NodeField, Any] = mutable.Map.empty
  data ++= entries

  def dataView: Map[NodeField, Any] = data.toMap

  def getFieldData(field: NodeField): FieldData = {
    val value = this(field)
    val source = sources.get(field)
    val data = fieldData.get(field)
    FieldData(value, source, data)
  }

  def setWithData(field: NodeField, data: FieldData): Unit = {
    this(field) = data.value
    data.range.foreach(r => this.sources(field) = r)
    data.fieldData.foreach(r => this.fieldData(field) = r)
  }

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
    def inner(visited: mutable.Set[Int]): String = {
      val hash = System.identityHashCode(this)
      if (visited.add(hash)) {
        val className = shape.toString
        if (data.isEmpty)
          return className
        return s"$className: ${data.
          filter(kv => !kv._1.isInstanceOf[TypedNodeField[_]]).
          map(kv => (kv._1.toString, kv._2))}"
      }
      "recursive"
    }

    var visible = visitedToString.get()
    if (visible != null)
      return inner(visible)

    visible = mutable.HashSet.empty
    visitedToString.set(visible)
    val result = inner(visible)
    visitedToString.set(null)
    result
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

  def range: Option[OffsetRange] =
    if (sources.values.isEmpty) None
    else Some(OffsetRange(
      sources.values.map(p => p.from.getAbsoluteOffset()).min,
      sources.values.map(p => p.until.getAbsoluteOffset()).max))

  override def getValue(key: NodeField): Any = get(key).get
}

object Node {
  val visitedToString = new ThreadLocal[mutable.Set[Int]]()
}

case class FieldData(value: Any, range: Option[OffsetNodeRange], fieldData: Option[mutable.Map[NodeField, Any]])


