package transformations.bytecode.constants

import core.grammarDocument.BiGrammar
import core.transformation.grammars.GrammarCatalogue
import core.transformation.{TransformationState, MetaObject}
import transformations.bytecode.PrintByteCode
import transformations.types.TypeC

object FieldDescriptorConstant extends ConstantEntry {
  object Key
  object Type

  def constructor(_type: MetaObject) = new MetaObject(Key, Type -> _type)

  override def key: Any = Key

  override def getByteCode(constant: MetaObject, state: TransformationState): Seq[Byte] = {
    val _type: MetaObject = constant(Type).asInstanceOf[MetaObject]
    val typeString = TypeC.getByteCodeString(state)(_type)
    PrintByteCode.toUTF8ConstantEntry(typeString)
  }

  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val typeGrammar = grammars.find(TypeC.TypeGrammar)
    "field:" ~> typeGrammar ^^ parseMap(Key, Type)
  }
}
