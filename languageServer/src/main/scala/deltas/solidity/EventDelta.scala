package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{PrimitiveType, TypeApplication}
import deltas.HasNameDelta
import deltas.HasNameDelta.{HasName, Name}
import deltas.bytecode.types.TypeSkeleton
import deltas.javac.classes.skeleton.{HasConstraintsDelta, JavaClassDelta}
import deltas.javac.methods.MethodParameters.MethodParameter
import deltas.javac.methods.{MethodDelta, MethodParameters}

object EventDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object Shape extends NodeShape
  object ParameterIndexed extends NodeField
  object Anonymous extends NodeField

  implicit class Event[T <: NodeLike](val node: T) extends HasName[T] {

    def parameters: Seq[MethodParameter[T]] = NodeWrapper.wrapList(node(MethodDelta.Parameters).asInstanceOf[Seq[T]])
    def parameters_=(value: Seq[MethodParameter[T]]): Unit = node(MethodDelta.Parameters) = NodeWrapper.unwrapList(value)
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)

    val parameter = typeGrammar.as(MethodParameters.Type) ~
      "indexed".spacedOption.as(ParameterIndexed) ~
      identifier.spacedOption.as(Name) asNode MethodParameters.Shape
    val parameterList = parameter.toParameterList.as(MethodDelta.Parameters)

    val grammar = "event" ~~ find(HasNameDelta.Name)  ~ parameterList ~~ "anonymous".option.as(Anonymous) ~ ";" asNode Shape
    find(JavaClassDelta.Members).addAlternative(grammar)
  }

  override def description = "Add events as contract members"

  override def dependencies = Set(SolidityContractDelta, TypeSkeleton)

  val eventConstructor = PrimitiveType("event")
  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val event: Event[NodePath] = path
    val parameterTypes = event.parameters.map(parameter => {
      val methodParameter: MethodParameter[NodePath] = parameter
      TypeSkeleton.getType(compilation, builder, methodParameter._type, parentScope)
    })
    val eventType = TypeApplication(eventConstructor, parameterTypes, path)
    builder.declareSourceElement(event.getSourceElement(HasNameDelta.Name), parentScope, Some(eventType))
  }

  override def shape = Shape
}
