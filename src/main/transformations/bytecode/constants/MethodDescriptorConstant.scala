package transformations.bytecode.constants

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.CompilationState
import core.particles.node.Node
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.types.TypeSkeleton

object MethodDescriptorConstant extends ConstantEntry {

  def methodDescriptor(returnDescriptor: Node, parameterDescriptors: Seq[Node]) = {
    new Node(MethodDescriptor,
      MethodDescriptorParameters -> parameterDescriptors,
      MethodReturnType -> returnDescriptor)
  }

  def getMethodDescriptorReturnType(descriptor: Node) = descriptor(MethodReturnType).asInstanceOf[Node]

  def getMethodDescriptorParameters(descriptor: Node) = descriptor(MethodDescriptorParameters).asInstanceOf[Seq[Node]]

  object MethodDescriptor

  object MethodDescriptorParameters

  object MethodReturnType

  override def key: Any = MethodDescriptor

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = {
    def javaTypeToString(_type: Node): String = TypeSkeleton.getByteCodeString(state)(_type)

    val returnString = javaTypeToString(getMethodDescriptorReturnType(constant))
    val parametersString = s"(${
      getMethodDescriptorParameters(constant).map(javaTypeToString).mkString("")
    })"
    toUTF8ConstantEntry(parametersString + returnString)
  }

  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val typeGrammar = grammars.find(TypeSkeleton.JavaTypeGrammar)
    (typeGrammar ~~ typeGrammar.manySeparated(";").inParenthesis) ^^
      parseMap(MethodDescriptor, MethodReturnType, MethodDescriptorParameters)
  }

  override def description: String = "Defines the method descriptor constant, which contains the type signature of the method."
}
