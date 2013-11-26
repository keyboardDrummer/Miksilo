package transformation

import scala.collection.mutable

class MetaObject(val clazz: String) {
  val data: mutable.Map[String,Any] = mutable.Map.empty

  def apply(key: String) = data(key)
  def apply(key: String, value: Any) = data.put(key, value)
}
