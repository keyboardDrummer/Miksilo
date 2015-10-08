package transformations.javac.types

import core.bigrammar.{BiFailure, BiGrammar}
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import transformations.bytecode.types.{TypeInstance, TypeSkeleton}


object MethodTypeC extends TypeInstance {

  implicit class MethodType(node: Node) {
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

  object MethodTypeKey extends Key

  object Parameters extends Key

  object ReturnType extends Key

  object ThrowsSignature extends Key

  val key: Key = MethodTypeKey

  override def description: String = "Defines the method type."

  override def getSuperTypes(_type: Node, state: CompilationState): Seq[Node] = ???

  override def getJavaGrammar(grammars: GrammarCatalogue): BiGrammar = BiFailure ///Deze heeft helemaal geen Java grammar.

  object ByteCodeMethodTypeGrammar
  override def getByteCodeGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val typeGrammar = grammars.find(TypeSkeleton.ByteCodeTypeGrammar)
    val throwsGrammar = ("^" ~> typeGrammar)*
    val methodGrammar = ("(" ~> (typeGrammar*) <~ ")") ~ typeGrammar ~ throwsGrammar ^^ parseMap(MethodTypeKey, Parameters, ReturnType, ThrowsSignature)
    grammars.create(ByteCodeMethodTypeGrammar, methodGrammar)
  }
}