package languages.java.base

import scala.collection.mutable


case class MethodRef(className: String, methodName: String)

class ConstantPool {
  val constants: Seq[Any] = mutable.Seq()
  val reverseRouter = mutable.Map[Any,Integer]()

  def storeMethodRef(ref: MethodRef) : Int = {
    val index = constants.length
    reverseRouter(ref) = index
    constants.+:(ref)
    index
  }

  def storeUtf8(value: String) = {
    val index = constants.length
    reverseRouter(value) = index
    constants +: value
    index
  }
}
