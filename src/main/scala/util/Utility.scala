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
}
