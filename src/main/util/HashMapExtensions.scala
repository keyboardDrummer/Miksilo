package util

import scala.collection.mutable


object HashMapExtensions {

  implicit def richMap[K, V](map: mutable.HashMap[K, V]) = new RichMap(map)

  class RichMap[K, V](map: mutable.HashMap[K, V]) {
    def putIfEmpty(key: K, value: V) = {
      if (!map.contains(key))
        map.put(key, value)
    }
  }

}
