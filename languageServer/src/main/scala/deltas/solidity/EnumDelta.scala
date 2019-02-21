package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{NodeField, NodeLike, NodeShape, NodeWrapper}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.DeclarationHasType
import core.smarts.types.objects.{Type, TypeFromDeclaration}
import deltas.HasNameDelta
import deltas.bytecode.types.TypeSkeleton
import deltas.javac.classes.skeleton.{HasConstraintsDelta, JavaClassDelta}

object EnumDelta extends DeltaWithGrammar with HasConstraintsDelta {

  import deltas.HasNameDelta._

  import deltas.HasNameDelta._

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
    find(JavaClassDelta.Members).addAlternative(grammar)
  }

  override def description = "Adds enums"

  override def dependencies = Set(SolidityContractDelta)

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val _enum: Enum[NodePath] = path
    val enumStaticScope = builder.newScope()
    val enumDeclaration = builder.declareSourceElement(path.getSourceElement(HasNameDelta.Name), parentScope)
    val enumStaticType: Type = TypeFromDeclaration(enumDeclaration) // TODO switch to using TypeFromScope
    builder.add(DeclarationHasType(enumDeclaration, enumStaticType))
    builder.assignSubType(TypeSkeleton.typeKind, enumStaticType)
    //val enumType = TypeFromDeclaration(enumDeclaration) TODO this we can do once we have TypeFromScope.
    for(value <- _enum.values) {
      builder.declareSourceElement(value.node.getSourceElement(HasNameDelta.Name), enumStaticScope) // , Some(enumType))
    }
  }

  override def shape = Shape
}
