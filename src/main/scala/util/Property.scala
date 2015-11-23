package util

trait Property[-TClass, T]
{
  def _type: Class[T]
  def get(obj: TClass): T
  def set(obj: TClass, value: T): Unit
}
