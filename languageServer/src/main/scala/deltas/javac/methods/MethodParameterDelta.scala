package deltas.javac.methods

import core.bigrammar.BiGrammar
import core.deltas.NodeGrammarWriter
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node._
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.bytecode.types.TypeSkeleton
import deltas.javac.types.TypeAbstraction.ParameterName

object MethodParameterDelta extends NodeGrammarWriter {

  def declare(compilation: Compilation, builder: ConstraintBuilder, parameter: MethodParameter[NodePath],
              parentScope: Scope,
              bodyScope: Scope): Unit = {

    val parameterType = TypeSkeleton.getType(compilation, builder, parameter._type, parentScope)
    val name = parameter(ParameterName).asInstanceOf[String]
    builder.declare(name, bodyScope, parameter.getLocation(ParameterName), Some(parameterType))
  }

  def getName(metaObject: Node): String = metaObject(Name).asInstanceOf[String]

  implicit class MethodParameter[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def _type: T = node(Type).asInstanceOf[T]
    def _type_=(value: T): Unit = node(Type) = value

    def name: Any = node(Name)
    def name_=(value: Any): Unit = node(Name) = value
  }

  def neww(name: String, _type: Any): Node = {
    new Node(Shape,
      Name -> name,
      Type -> _type)
  }

  def getParameterGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._

    val parseType = find(TypeSkeleton.JavaTypeGrammar)
    parseType.as(Type) ~~ identifier.as(Name) asNode Shape
  }

  object Shape extends NodeShape

  object Name extends NodeField

  object Type extends NodeField
}
