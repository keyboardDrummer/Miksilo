package transformations.bytecode.constants

import core.grammarDocument.BiGrammar
import core.transformation.grammars.GrammarCatalogue
import core.transformation.{TransformationState, MetaObject}
import transformations.bytecode.PrintByteCode._

object NameAndType extends ConstantEntry {

  object NameAndTypeKey

  object NameAndTypeName

  object NameAndTypeType

  def nameAndType(nameIndex: Int, typeIndex: Int): MetaObject = new MetaObject(NameAndTypeKey) {
    data.put(NameAndTypeName, nameIndex)
    data.put(NameAndTypeType, typeIndex)
  }

  def getNameAndTypeName(nameAndType: MetaObject) = nameAndType(NameAndTypeName).asInstanceOf[Int]

  def getTypeIndex(nameAndType: MetaObject) = nameAndType(NameAndTypeType).asInstanceOf[Int]

  override def key: Any = NameAndTypeKey

  override def getByteCode(constant: MetaObject, state: TransformationState): Seq[Byte] = {
    byteToBytes(12) ++ shortToBytes(getNameAndTypeName(constant)) ++
      shortToBytes(getTypeIndex(constant))
  }

  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = "name and type:" ~~> (integer <~ ":") ~ integer ^^
    parseMap(NameAndTypeKey, NameAndTypeName, NameAndTypeType)
}
