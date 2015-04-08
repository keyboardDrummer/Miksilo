package transformations.javac.classes

import core.particles.node.Node
import transformations.bytecode.constants.ClassRefConstant
import transformations.javac.classes.skeleton.QualifiedClassName

import scala.collection.mutable

class ConstantPool(items: Seq[Any] = Seq.empty) {
  val constants: mutable.Buffer[Any] = new mutable.ArrayBuffer[Any]()
  val reverseRouter = mutable.Map[Any, Int]()
  items.foreach(store)

  def getNode(index: Int) = getValue(index).asInstanceOf[Node]
  def getUtf8(index: Int) = getValue(index).asInstanceOf[String]
  def getValue(index: Int) = constants(index - 1)

  def getClassRef(nameParts: QualifiedClassName): Int = {
    val nameIndex = store(nameParts)
    store(ClassRefConstant.classRef(nameIndex))
  }

  def store(ref: Any): Int = {
    val result = reverseRouter.getOrElse[Int](ref, {
      val index = constants.length + 1
      reverseRouter(ref) = index
      constants.append(ref)
      index
    })
    if (ref.isInstanceOf[Long] || ref.isInstanceOf[Double])
      store(Hole)
    result
  }

  object Hole

  def constantSize(constant: Any): Int = {
    if (constant.isInstanceOf[Long] || constant.isInstanceOf[Double]) 2 else 1
  }

  def storeUtf8(value: String) = {
    store(value)
  }
}
