package transformations.bytecode.constants

import core.grammarDocument.BiGrammar
import core.transformation.grammars.GrammarCatalogue
import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.PrintByteCode._
import transformations.types.TypeC

object MethodDescriptorConstant extends ConstantEntry {

  def methodDescriptor(returnDescriptor: MetaObject, parameterDescriptors: Seq[MetaObject]) = {
    new MetaObject(MethodDescriptor) {
      data.put(MethodDescriptorParameters, parameterDescriptors)
      data.put(MethodReturnType, returnDescriptor)
    }
  }

  def getMethodDescriptorReturnType(descriptor: MetaObject) = descriptor(MethodReturnType).asInstanceOf[MetaObject]

  def getMethodDescriptorParameters(descriptor: MetaObject) = descriptor(MethodDescriptorParameters).asInstanceOf[Seq[MetaObject]]

  object MethodDescriptor

  object MethodDescriptorParameters

  object MethodReturnType

  override def key: Any = MethodDescriptor

  override def getByteCode(constant: MetaObject, state: TransformationState): Seq[Byte] = {
    def javaTypeToString(_type: MetaObject): String = TypeC.getByteCodeString(state)(_type)

    val returnString = javaTypeToString(getMethodDescriptorReturnType(constant))
    val parametersString = s"(${
      getMethodDescriptorParameters(constant).map(javaTypeToString).mkString("")
    })"
    toUTF8ConstantEntry(parametersString + returnString)
  }

  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val typeGrammar = grammars.find(TypeC.TypeGrammar)
    (typeGrammar ~~ typeGrammar.manySeparated(";").inParenthesis) ^^
      parseMap(MethodDescriptor, MethodReturnType, MethodDescriptorParameters)
  }
}
