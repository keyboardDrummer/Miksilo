package transformation

import scala.collection.mutable

object MetaObject {

  def deepEquality(first: MetaObject, second: MetaObject) : Boolean = {
    deepEquality(first,second, mutable.Set[(MetaObject,MetaObject)]())
  }

  private def deepEquality(first: Any, second: Any, closed: mutable.Set[(MetaObject,MetaObject)]) : Boolean = {
    if (first == second)
      return true

    (first,second) match {
      case (seq1: Seq[_], seq2: Seq[_]) => seq1.zip(seq2).forall(p => deepEquality(p._1,p._2, closed))
      case (meta1:MetaObject,meta2:MetaObject) => deepEquality(meta1,meta2,closed)
      case _ => false
    }
  }

  private def deepEquality(first: MetaObject, second: MetaObject, closed: mutable.Set[(MetaObject,MetaObject)]) : Boolean = {
    val key = (first,second)
    if (!closed.add(key))
      true

    if (!first.clazz.equals(second.clazz))
      false

    val sharedKeys = first.data.keySet ++ second.data.keySet
    sharedKeys.forall(key => (first.data.get(key),second.data.get(key)) match {
      case (Some(firstVal),Some(secondVal)) => deepEquality(firstVal, secondVal, closed)
      case _ => false
    })
  }
}

class MetaObject(val clazz: AnyRef) {
  val data: mutable.Map[Any,Any] = mutable.Map.empty

  def apply(key: Any) = data(key)
  def update(key: Any, value: Any) = data.put(key, value)
}
