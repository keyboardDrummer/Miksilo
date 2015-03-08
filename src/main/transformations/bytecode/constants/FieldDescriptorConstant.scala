package transformations.bytecode.constants

import core.grammarDocument.BiGrammar
import core.transformation.grammars.GrammarCatalogue
import core.transformation.{CompilationState, MetaObject}
import transformations.bytecode.PrintByteCode
import transformations.types.TypeSkeleton

object FieldDescriptorConstant extends ConstantEntry {
  object Key
  object Type

  def constructor(_type: MetaObject) = new MetaObject(Key, Type -> _type)

  override def key: Any = Key

  override def getByteCode(constant: MetaObject, state: CompilationState): Seq[Byte] = {
    val _type: MetaObject = constant(Type).asInstanceOf[MetaObject]
    val typeString = TypeSkeleton.getByteCodeString(state)(_type)
    PrintByteCode.toUTF8ConstantEntry(typeString)
  }

  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val typeGrammar = grammars.find(TypeSkeleton.TypeGrammar)
    "field:" ~> typeGrammar ^^ parseMap(Key, Type)
  }

  override def description: String = "Adds the field descriptor constant. It contains the type of a field."
}
