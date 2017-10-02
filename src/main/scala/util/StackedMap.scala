package util

import scala.collection.mutable

class StackedMap[K,V] extends mutable.Map[K,V] {
  var stack = List.empty[mutable.Map[K,V]]

  def push(): Unit = stack ::= new mutable.HashMap[K,V]()
  def pop(): Unit = stack = stack.tail

  def current: mutable.Map[K, V] = stack.head
  def +=(kv: (K, V)): this.type = { current.+=(kv); this }

  def iterator: Iterator[(K, V)] = {
    val keys = stack.map(map => map.keySet).fold(Set.empty[K])((a,b) => a.union(b))
    keys.map(key => (key,this(key))).iterator
  }

  def get(key: K): Option[V] = stack.flatMap(map => map.get(key)).headOption

  def -=(key: K): this.type = throw new NotImplementedError()
}