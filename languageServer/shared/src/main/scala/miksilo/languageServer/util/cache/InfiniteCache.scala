package miksilo.languageServer.util.cache

import scala.collection.mutable

class InfiniteCache[Key, Value] extends Cache[Key, Value] {
  private val data = mutable.Map[Key, Value]()

  override def get(node: Key): Option[Value] = {
    data.get(node)
  }

  override def add(node: Key, value: Value): Unit = {
    data.put(node, value)
  }

  override def size: Int = data.size
}
