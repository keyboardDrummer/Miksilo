package deltas.javac.classes

import core.language.node.Node
import deltas.bytecode.constants.{ClassInfoConstant, DoubleInfoConstant, LongInfoConstant, Utf8ConstantDelta}
import deltas.javac.classes.skeleton.QualifiedClassName

import scala.collection.mutable

class ConstantPool(items: Seq[Any] = Seq.empty) {
  val constants: mutable.Buffer[Any] = new mutable.ArrayBuffer[Any]()
  val reverseRouter = mutable.Map[Any, Int]()
  items.foreach(store)

  def getNode(index: Int) = getValue(index).asInstanceOf[Node]
  def getUtf8(index: Int) = Utf8ConstantDelta.get(getValue(index).asInstanceOf[Node])
  def getValue(index: Int) = constants(index - 1)

  def getClassRef(nameParts: QualifiedClassName): Int = {
    val nameIndex = store(nameParts)
    store(ClassInfoConstant.classRef(nameIndex))
  }

  def store(ref: Any): Int = {
    val result = reverseRouter.getOrElse[Int](ref, {
      val index = constants.length + 1
      reverseRouter(ref) = index
      constants.append(ref)
      index
    })
    ref match {
      case node:Node => node.shape match {
        case LongInfoConstant.LongEntryKey => store(new Hole())
        case DoubleInfoConstant.DoubleEntryKey => store(new Hole())
        case _ =>
      }
      case _ =>
    }
    result
  }

  class Hole

  def storeUtf8(value: String) = {
    store(value)
  }

  override def toString = "ConstantPool: " + items.toString()
}
