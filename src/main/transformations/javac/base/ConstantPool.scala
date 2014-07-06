package transformations.javac.base

import scala.collection.mutable

class ConstantPool(val constants: mutable.Buffer[Any] = mutable.Buffer()) {
  val reverseRouter = mutable.Map[Any,Int]()
  for(indexedConstant <- constants.zipWithIndex)
    reverseRouter(indexedConstant._1) = indexedConstant._2

  def getValue(index: Int) = constants(index - 1)

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
