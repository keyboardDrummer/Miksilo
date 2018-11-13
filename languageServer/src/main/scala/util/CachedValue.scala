package util


class CachedValue[T >: Null <: AnyRef](supplier: () => T) {
  var value: T = _

  def get: T = {
    if (value == null) {
      value = supplier()
      return value
    }
    value
  }

  def clear() : Unit = value = null
}