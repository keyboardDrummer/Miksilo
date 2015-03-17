package util

object Utility {
  def repeat[T](seed: T, function: T => T): Stream[T] = {
    seed #:: repeat(function(seed),function)
  }
}
