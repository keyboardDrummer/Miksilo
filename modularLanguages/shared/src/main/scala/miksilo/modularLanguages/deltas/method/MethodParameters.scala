package miksilo.modularLanguages.deltas.javac.methods

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.NodeGrammarWriter
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node._
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.bytecode.types.TypeSkeleton

object MethodParameters {

  import miksilo.modularLanguages.deltas.HasNameDelta._

  def declare(compilation: Compilation, builder: ConstraintBuilder, parameter: MethodParameter[NodePath],
              parentScope: Scope,
              bodyScope: Scope): Unit = {

    val parameterType = TypeSkeleton.getType(compilation, builder, parameter._type, parentScope)
    val name = parameter.getValue(Name).asInstanceOf[String]
    builder.declare(name, bodyScope, parameter.getField(Name), Some(parameterType))
  }

  implicit class MethodParameter[T <: NodeLike](val node: T) extends NodeWrapper[T] with HasName[T] {
    def _type: T = node(Type).asInstanceOf[T]
    def _type_=(value: T): Unit = node(Type) = value
  }

  def neww(name: String, _type: Any): Node = {
    new Node(Shape,
      Name -> name,
      Type -> _type)
  }

  def getGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._

    val parseType = find(TypeSkeleton.JavaTypeGrammar)
    parseType.as(Type) ~~ find(Name) asNode Shape
  }

  object Shape extends NodeShape

  object Type extends NodeField
}
