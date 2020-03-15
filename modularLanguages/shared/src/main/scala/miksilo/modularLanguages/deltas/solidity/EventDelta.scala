package miksilo.modularLanguages.deltas.solidity

import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node._
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.{PrimitiveType, TypeApplication}
import miksilo.modularLanguages.deltas.HasNameDelta
import miksilo.modularLanguages.deltas.HasNameDelta.{HasName, Name}
import miksilo.modularLanguages.deltas.bytecode.types.TypeSkeleton
import miksilo.modularLanguages.core.deltas.path.ConstraintBuilderExtension._
import miksilo.modularLanguages.deltas.classes.ClassDelta
import miksilo.modularLanguages.deltas.javac.classes.skeleton.{HasConstraintsDelta}
import miksilo.modularLanguages.deltas.javac.methods.{MethodDelta, MethodParameters}
import miksilo.modularLanguages.deltas.javac.methods.MethodParameters.MethodParameter

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
    find(ClassDelta.Members).addAlternative(grammar)
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
    builder.declare(event.getField(HasNameDelta.Name), parentScope, eventType)
  }

  override def shape = Shape
}
