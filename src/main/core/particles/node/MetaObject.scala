package core.particles.node

import core.particles.path.Path

import scala.collection.mutable



object MetaObject {

  def classDebugRepresentation(_clazz: Any): String = _clazz match {
    case string: String => string
    case anyRef: AnyRef =>
      val simpleName: String = anyRef.getClass.getSimpleName
      if (simpleName.last == '$')
        return simpleName.dropRight(1)
      simpleName
    case _ => _clazz.toString
  }
}

class MetaObject(var clazz: AnyRef, entries: (Any, Any)*) extends Dynamic with MetaLikeGen[MetaObject] {

  def replaceWith(metaObject: MetaObject): Unit = {
    clazz = metaObject.clazz
    data.clear()
    data ++= metaObject.data
  }

  val data: mutable.Map[Any, Any] = mutable.Map.empty
  data ++= entries

  def dataView = data.toMap

  def apply(key: Any) = data(key)

  def update(key: Any, value: Any) = {
    value match //TODO maybe throw this check away.
    {
      case wrong: Path => throwInsertedWithOriginIntoRegularMetaObject()
      case sequence: Seq[_] => if (!sequence.forall(item => !item.isInstanceOf[Path]))
        throwInsertedWithOriginIntoRegularMetaObject()
      case _ =>
    }

    data.put(key, value)
  }

  def throwInsertedWithOriginIntoRegularMetaObject(): Unit = {
    throw new scala.UnsupportedOperationException("Don't insert a Origin into a regular MetaObject.")
  }

  def selectDynamic(name: String) =
    data.getOrElse(name, sys.error("member not found"))

  def updateDynamic(name: String)(value: Any) {
    data += name -> value
  }

  override def toString: String = {
    val className = MetaObject.classDebugRepresentation(clazz)
    if (data.isEmpty)
      return className
    s"$className: ${data.map(kv => (MetaObject.classDebugRepresentation(kv._1), kv._2))}"
  }

  override def equals(other: Any): Boolean = other match {
    case that: MetaObject =>
      val dataEquals: Boolean = data == that.data
      (that canEqual this) &&
        dataEquals &&
        clazz == that.clazz
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[MetaObject]

  override def hashCode(): Int = {
    val state = Seq(data, clazz)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def get(key: Any): Option[Any] = data.get(key)
}


