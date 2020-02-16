package util.cache

trait Cache[Key, Value] {
  def get(key: Key): Option[Value]
  def add(key: Key, value: Value): Unit
  def size: Int
}