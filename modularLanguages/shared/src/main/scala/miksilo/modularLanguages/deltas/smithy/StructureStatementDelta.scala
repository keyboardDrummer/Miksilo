package miksilo.modularLanguages.deltas.smithy

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node.{NodeField, NodeLike, NodeShape, NodeWrapper}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.smithy.RelativeShapeIdentifierDelta.shapeType
import miksilo.modularLanguages.deltas.{ConstraintSkeleton, FileWithMembersDelta, HasNameDelta}
import miksilo.modularLanguages.core.deltas.path.ConstraintBuilderExtension._
import miksilo.modularLanguages.deltas.classes.HasConstraintsDelta

object StructureStatementDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object Shape extends NodeShape
  object Members extends NodeField

  object MemberShape extends NodeShape
  object MemberType extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val name = find(HasNameDelta.Name)
    val shapeIdentifier = find(RelativeShapeIdentifierDelta.ShapeIdentifierGrammar)
    val traits = find(TraitDelta.Traits)
    val structuredMember: BiGrammar = traits.option % name ~ ":" ~~ shapeIdentifier.as(MemberType) asNode MemberShape
    val trailingComma = ",".option
    val structureBody = name ~~ (structuredMember.manySeparatedVertical(",").as(Members) ~ trailingComma).inBraces
    val grammar = "structure" ~~ structureBody asNode Shape
    val members = find(ShapeStatementDelta.ShapeBody)
    members.addAlternative(grammar)
  }

  override def description = "Adds structure statement delta"

  override def dependencies = Set(FileWithMembersDelta)

  override def shape = Shape

  implicit class Structure[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def members: Seq[StructureMember[T]] = NodeWrapper.wrapList(node(Members).asInstanceOf[Seq[T]])
  }

  implicit class StructureMember[T <: NodeLike](val node: T) extends HasNameDelta.HasName[T] {
    def _type: T = node(MemberType).asInstanceOf[T]
  }

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder,
                                  path: NodePath, parentScope: Scope): Unit = {
    val structureDeclaration = builder.declare(path.getField(HasNameDelta.Name), parentScope,
      RelativeShapeIdentifierDelta.shapeType)
    val structureScope = builder.declareScope(structureDeclaration)

    val structure: Structure[NodePath] = path
    structure.members.foreach(member => {
      builder.declare(member.getField(HasNameDelta.Name), structureScope, shapeType)
      ConstraintSkeleton.constraints(compilation, builder, member._type, parentScope)
    })
  }
}
