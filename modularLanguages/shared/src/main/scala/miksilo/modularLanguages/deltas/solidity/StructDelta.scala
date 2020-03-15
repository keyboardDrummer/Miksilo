package miksilo.modularLanguages.deltas.solidity

import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node.{NodeField, NodeLike, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.{ConstraintSkeleton, HasNameDelta}
import miksilo.modularLanguages.deltas.HasNameDelta.HasName
import miksilo.modularLanguages.deltas.bytecode.types.TypeSkeleton
import miksilo.modularLanguages.deltas.statement.LocalDeclarationDelta
import miksilo.modularLanguages.core.deltas.path.ConstraintBuilderExtension._
import miksilo.modularLanguages.deltas.classes.ClassDelta
import miksilo.modularLanguages.deltas.javac.classes.skeleton.HasConstraintsDelta

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
    find(ClassDelta.Members).addAlternative(grammar)
  }

  override def description = "Adds solidity structs"

  override def dependencies = Set(SolidityContractDelta)

  override def shape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val struct: Struct[NodePath] = path
    val declaration = builder.declare(path.getField(HasNameDelta.Name), parentScope, TypeSkeleton.typeKind)
    val structScope = builder.declareScope(declaration, parentScope, s"struct '${struct.name}'")
    for(member <- struct.members) {
      ConstraintSkeleton.constraints(compilation, builder, member, structScope)
    }
  }
}
