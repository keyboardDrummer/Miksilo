package util

import scala.reflect.ClassTag

object Utility {
  def repeat[T](seed: T, function: T => T): Stream[T] = {
    seed #:: repeat(function(seed),function)
  }

  def cast[T: ClassTag](o: Any): Option[T] = o match {
    case v: T => Some(v)
    case _ => None
  }

  def mergeMaps[Key, Value](first: Map[Key, Value], second: Map[Key, Value],
                            combine: (Key, Value, Value) => Value): Map[Key, Value] = {
    var result: Map[Key, Value] = first
    for((secondKey, secondValue) <- second) {
      result += (secondKey -> (first.get(secondKey) match {
        case None => second(secondKey)
        case Some(firstValue) => combine(secondKey, firstValue, secondValue)
      }))
    }
    result
  }
}
