package languages.javac.base

import scala.collection.mutable

class ConstantPool {
  val constants: mutable.Buffer[Any] = mutable.Buffer()
  val reverseRouter = mutable.Map[Any,Int]()

  def store(ref: Any) : Int = {
    reverseRouter.getOrElse[Int](ref, {
      val index = constants.length + 1
      reverseRouter(ref) = index
      constants.append(ref)
      index
    })
  }

  def storeUtf8(value: String) = {
    store(value)
  }
}
