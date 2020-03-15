package miksilo.modularLanguages.deltas.solidity

import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node.{NodeField, NodeLike, NodeShape, NodeWrapper}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.DeclarationHasType
import miksilo.languageServer.core.smarts.types.objects.{Type, TypeFromDeclaration}
import miksilo.modularLanguages.deltas.HasNameDelta
import miksilo.modularLanguages.deltas.bytecode.types.TypeSkeleton
import miksilo.modularLanguages.core.deltas.path.ConstraintBuilderExtension._
import miksilo.modularLanguages.deltas.classes.ClassDelta
import miksilo.modularLanguages.deltas.classes.HasConstraintsDelta

object EnumDelta extends DeltaWithGrammar with HasConstraintsDelta {

  import miksilo.modularLanguages.deltas.HasNameDelta._

  object Shape extends NodeShape
  object Values extends NodeField

  object ValueShape extends NodeShape

  implicit class Enum[T <: NodeLike](val node: T) extends HasName[T] {
    def values: Seq[EnumValue[T]] = NodeWrapper.wrapList(node(Values).asInstanceOf[Seq[T]])
  }

  implicit class EnumValue[T <: NodeLike](val node: T) extends HasName[T] {
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val name = find(Name)
    val valueGrammar = name.asNode(ValueShape)
    val grammar = "enum" ~~ name ~ "{" ~ valueGrammar.manySeparated("," ~ printSpace).as(Values) ~ "}" asNode Shape
    find(ClassDelta.Members).addAlternative(grammar)
  }

  override def description = "Adds enums"

  override def dependencies = Set(SolidityContractDelta)

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val _enum: Enum[NodePath] = path
    val enumStaticScope = builder.newScope()
    val enumDeclaration = builder.declare(path.getField(HasNameDelta.Name), parentScope)
    val enumStaticType: Type = TypeFromDeclaration(enumDeclaration) // TODO switch to using TypeFromScope
    builder.add(DeclarationHasType(enumDeclaration, enumStaticType))
    builder.assignSubType(TypeSkeleton.typeKind, enumStaticType)
    //val enumType = TypeFromDeclaration(enumDeclaration) TODO this we can do once we have TypeFromScope.
    for(value <- _enum.values) {
      builder.declare(value.node.getField(HasNameDelta.Name), enumStaticScope) // , Some(enumType))
    }
  }

  override def shape = Shape
}
