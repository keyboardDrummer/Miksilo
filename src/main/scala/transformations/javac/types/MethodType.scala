package transformations.javac.types

import core.bigrammar.{BiFailure, BiGrammar}
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeClass, NodeField}
import transformations.bytecode.types.{TypeInstance, TypeSkeleton}


object MethodType extends TypeInstance {

  implicit class MethodTypeWrapper(node: Node) {
    def returnType: Node = node(ReturnType).asInstanceOf[Node]
    def returnType_=(value: Node) = node(ReturnType) = value

    def parameterTypes: Seq[Node] = node(Parameters).asInstanceOf[Seq[Node]]
    def parameterTypes_=(value: Seq[Node]) = node(Parameters) = value
  }

  def construct(returnType: Node, parameterTypes: Seq[Node]) = {
    new Node(MethodTypeKey,
      Parameters -> parameterTypes,
      ReturnType -> returnType,
      ThrowsSignature -> Seq.empty[Node])
  }

  object MethodTypeKey extends NodeClass

  object Parameters extends NodeField

  object ReturnType extends NodeField

  object ThrowsSignature extends NodeField

  val key: Key = MethodTypeKey

  override def description: String = "Defines the method type."

  override def getSuperTypes(_type: Node, state: CompilationState): Seq[Node] = ???

  override def getJavaGrammar(grammars: GrammarCatalogue): BiGrammar = BiFailure ///Deze heeft helemaal geen Java grammar.

  override def getByteCodeGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val typeGrammar = grammars.find(TypeSkeleton.ByteCodeTypeGrammar)
    val throwsGrammar = ("^" ~> typeGrammar)*
    val methodGrammar = (("(" ~> (typeGrammar*).as(Parameters) <~ ")") ~ typeGrammar.as(ReturnType) ~ throwsGrammar.as(ThrowsSignature)).asNode(MethodTypeKey)
    methodGrammar
  }
}