package util

import java.lang.reflect.Method

class GetterSetterProperty[-TClass, T <: AnyRef](val getter: Method, setter: Method) extends Property[TClass, T] {

  val propertyType = getter.getReturnType

  override def get(obj: TClass): T = getter.invoke(obj).asInstanceOf[T]

  override def set(obj: TClass, value: T): Unit = setter.invoke(obj, value)

  override def _type: Class[T] = propertyType.asInstanceOf[Class[T]]

  def canEqual(other: Any): Boolean = other.isInstanceOf[GetterSetterProperty[TClass, T]]

  override def equals(other: Any): Boolean = other match {
    case that: GetterSetterProperty[TClass, T] =>
      (that canEqual this) &&
        getter == that.getter
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(getter)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

trait Property[-TClass, T]
{
  def _type: Class[T]
  def get(obj: TClass): T
  def set(obj: TClass, value: T): Unit
}
