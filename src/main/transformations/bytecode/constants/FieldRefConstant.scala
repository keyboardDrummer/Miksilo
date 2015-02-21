package transformations.bytecode.constants

import core.grammarDocument.BiGrammar
import core.transformation.grammars.GrammarCatalogue
import core.transformation.{TransformationState, MetaObject}
import transformations.bytecode.PrintByteCode._

object FieldRefConstant extends ConstantEntry {

  object FieldRef

  object FieldRefClassIndex

  object FieldRefNameAndTypeIndex

  def fieldRef(classIndex: Int, nameAndTypeIndex: Int) = new MetaObject(FieldRef) {
    data.put(FieldRefClassIndex, classIndex)
    data.put(FieldRefNameAndTypeIndex, nameAndTypeIndex)
  }

  override def getByteCode(constant: MetaObject, state: TransformationState): Seq[Byte] = {
    byteToBytes(9) ++
      shortToBytes(getFieldRefClassIndex(constant)) ++
      shortToBytes(getFieldRefNameAndTypeIndex(constant))
  }

  override def key: Any = FieldRef

  def getFieldRefClassIndex(fieldRef: MetaObject) = fieldRef(FieldRefClassIndex).asInstanceOf[Int]

  def getFieldRefNameAndTypeIndex(fieldRef: MetaObject) = fieldRef(FieldRefNameAndTypeIndex).asInstanceOf[Int]

  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = "field reference:" ~~> (integer <~ ".") ~ integer ^^ parseMap(FieldRef, FieldRefClassIndex, FieldRefNameAndTypeIndex)
}
