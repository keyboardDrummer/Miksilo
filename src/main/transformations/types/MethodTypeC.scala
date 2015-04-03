package transformations.types

import core.bigrammar.BiGrammar
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}


object MethodTypeC extends TypeInstance {

  def construct(returnDescriptor: Node, parameterDescriptors: Seq[Node]) = {
    new Node(MethodTypeKey,
      Parameters -> parameterDescriptors,
      ReturnType -> returnDescriptor)
  }

  def getReturnType(descriptor: Node) = descriptor(ReturnType).asInstanceOf[Node]

  def getParameters(descriptor: Node) = descriptor(Parameters).asInstanceOf[Seq[Node]]

  object MethodTypeKey extends Key

  object Parameters extends Key

  object ReturnType extends Key

  val key: Key = MethodTypeKey

  override def description: String = "Defines the method type."

  override def getSuperTypes(_type: Node, state: CompilationState): Seq[Node] = ???

  override def getJavaGrammar(grammars: GrammarCatalogue): BiGrammar = "jerp" ^^ parseMap(key)

  object ByteCodeMethodTypeGrammar
  override def getByteCodeGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val typeGrammar = grammars.find(TypeSkeleton.ByteCodeTypeGrammar)
    val methodGrammar = ("(" ~> (typeGrammar*) <~ ")") ~ typeGrammar ^^ parseMap(MethodTypeKey, Parameters, ReturnType)
    grammars.create(ByteCodeMethodTypeGrammar, methodGrammar)
  }
}