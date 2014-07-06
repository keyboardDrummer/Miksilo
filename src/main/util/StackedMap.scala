package util

import scala.collection.mutable

class StackedMap[K,V] extends mutable.Map[K,V] {
  val stack = new mutable.Stack[mutable.Map[K,V]]()

  def push() = stack.push(new mutable.HashMap[K,V]())
  def pop() = stack.pop()

  def current = stack.top
  def +=(kv: (K, V)): this.type = { current.+=(kv); this }

  def iterator: Iterator[(K, V)] = {
    val keys = stack.map(map => map.keySet).fold(Set.empty[K])((a,b) => a.union(b))
    keys.map(key => (key,this(key))).iterator
  }

  def get(key: K): Option[V] = stack.map(map => map.get(key)).flatten.headOption

  def -=(key: K): this.type = throw new NotImplementedError()
}