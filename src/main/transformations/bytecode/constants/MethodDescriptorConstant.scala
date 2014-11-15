package transformations.bytecode.constants

import core.grammarDocument.BiGrammar
import core.transformation.MetaObject
import transformations.bytecode.ByteCodeSkeleton._
import transformations.types.TypeC

trait MethodDescriptorConstant {

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

  def getMethodDescriptorGrammar(typeGrammar: BiGrammar) = (typeGrammar ~~ typeGrammar.manySeparated(";").inParenthesis) ^^
    parseMap(MethodDescriptor, MethodReturnType, MethodDescriptorParameters)
}
