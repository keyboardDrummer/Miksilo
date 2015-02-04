package transformations.bytecode.constants

import core.transformation.MetaObject
import transformations.bytecode.ByteCodeSkeleton._

trait FieldRefConstant {

  object FieldRef

  object FieldRefClassIndex

  object FieldRefNameAndTypeIndex

  def fieldRef(classIndex: Int, nameAndTypeIndex: Int) = new MetaObject(FieldRef) {
    data.put(FieldRefClassIndex, classIndex)
    data.put(FieldRefNameAndTypeIndex, nameAndTypeIndex)
  }

  def getFieldRefClassIndex(fieldRef: MetaObject) = fieldRef(FieldRefClassIndex).asInstanceOf[Int]

  def getFieldRefNameAndTypeIndex(fieldRef: MetaObject) = fieldRef(FieldRefNameAndTypeIndex).asInstanceOf[Int]

  val fieldRefGrammar = "field reference:" ~~> (integer <~ ".") ~ integer ^^ parseMap(FieldRef, FieldRefClassIndex, FieldRefNameAndTypeIndex)
}
