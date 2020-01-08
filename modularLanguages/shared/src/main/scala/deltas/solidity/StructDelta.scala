package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{NodeField, NodeLike, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.{ConstraintSkeleton, HasNameDelta}
import deltas.javac.classes.skeleton.HasConstraintsDelta
import deltas.HasNameDelta.HasName
import deltas.bytecode.types.TypeSkeleton
import deltas.statement.LocalDeclarationDelta
import core.deltas.path.ConstraintBuilderExtension._
import deltas.javac.classes.skeleton.JavaClassDelta

object StructDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object Shape extends NodeShape
  object Members extends NodeField

  implicit class Struct[T <: NodeLike](val node: T) extends HasName[T] {
    def members = node(Members).asInstanceOf[Seq[T]]
    def members_=(value: Seq[T]): Unit = node(Members) = value
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val declaration = find(LocalDeclarationDelta.Shape)
    val grammar = "struct" ~~ find(HasNameDelta.Name) ~ "{" % declaration.manyVertical.as(Members) % "}" asNode Shape
    find(JavaClassDelta.Members).addAlternative(grammar)
  }

  override def description = "Adds solidity structs"

  override def dependencies = Set(SolidityContractDelta)

  override def shape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val struct: Struct[NodePath] = path
    val declaration = builder.declare(path.getField(HasNameDelta.Name), parentScope, TypeSkeleton.typeKind)
    val structScope = builder.declareScope(declaration, Some(parentScope), s"struct '${struct.name}'")
    for(member <- struct.members) {
      ConstraintSkeleton.constraints(compilation, builder, member, structScope)
    }
  }
}
