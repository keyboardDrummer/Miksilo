package deltas.javac.types

import core.bigrammar.grammars.BiFailure
import core.bigrammar.BiGrammar
import core.deltas.Compilation
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.deltas.path.NodePath
import core.language.Language
import core.nabl.ConstraintBuilder
import core.nabl.scopes.objects.Scope
import core.nabl.types.objects.{FunctionType, Type}
import deltas.bytecode.types.{TypeInstance, TypeSkeleton}

object MethodType extends TypeInstance {

  implicit class MethodTypeWrapper[T <: NodeLike](node: T) {
    def returnType: T = node(ReturnType).asInstanceOf[T]
    def returnType_=(value: T) = node(ReturnType) = value

    def parameterTypes: Seq[T] = node(Parameters).asInstanceOf[Seq[T]]
    def parameterTypes_=(value: Seq[T]) = node(Parameters) = value
  }

  def construct(returnType: Node, parameterTypes: Seq[Node]) = {
    new Node(MethodTypeKey,
      Parameters -> parameterTypes,
      ReturnType -> returnType,
      ThrowsSignature -> Seq.empty[Node])
  }

  object MethodTypeKey extends NodeShape

  object Parameters extends NodeField

  object ReturnType extends NodeField

  object ThrowsSignature extends NodeField

  val key = MethodTypeKey

  override def description: String = "Defines the method type."

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = ???

  override def getJavaGrammar(grammars: LanguageGrammars): BiGrammar = BiFailure()

  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    val typeGrammar = find(TypeSkeleton.ByteCodeTypeGrammar)
    val throwsGrammar = ("^" ~> typeGrammar)*
    val methodGrammar = (("(" ~> (typeGrammar*).as(Parameters) ~< ")") ~ typeGrammar.as(ReturnType) ~ throwsGrammar.as(ThrowsSignature)).asNode(MethodTypeKey)
    methodGrammar
  }

  override def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodePath, parentScope: Scope): Type = {
    val parameters = _type.parameterTypes
    val returnType = _type.returnType

    var result: Type = TypeSkeleton.getType(compilation, builder, returnType, parentScope)
    for(parameter <- parameters.reverse) {
      val parameterType = TypeSkeleton.getType(compilation, builder, parameter, parentScope)
      result = FunctionType(parameterType, result, _type)
    }
    result
  }
}