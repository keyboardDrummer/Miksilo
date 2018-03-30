package util


class Cache[T >: Null <: AnyRef](supplier: () => T) {
  var value: T = null

  def get: T = {
    if (value == null) {
      value = supplier()
      return value
    }
    value
  }

  def clear() : Unit = value = null
}