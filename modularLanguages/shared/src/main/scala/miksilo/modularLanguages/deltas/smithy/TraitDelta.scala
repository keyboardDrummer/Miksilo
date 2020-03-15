package miksilo.modularLanguages.deltas.smithy

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node.{NodeField, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.PrimitiveType
import miksilo.modularLanguages.deltas.expression.ExpressionDelta
import miksilo.modularLanguages.deltas.json.JsonObjectLiteralDelta
import miksilo.modularLanguages.deltas.{ConstraintSkeleton, FileWithMembersDelta, HasNameDelta}
import miksilo.modularLanguages.core.deltas.path.ConstraintBuilderExtension._
import miksilo.modularLanguages.deltas.javac.classes.skeleton.HasConstraints

object TraitDelta extends DeltaWithGrammar {

  object TraitValueShape extends NodeShape
  object TraitReference extends NodeField

  val traitType = PrimitiveType("trait")

  object TraitStatement extends NodeShape

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val objectValue = find(JsonObjectLiteralDelta.Shape)
    val name = find(HasNameDelta.Name)
    val traitStatement = "trait" ~~ name ~~ objectValue
    val members = find(FileWithMembersDelta.Members)
    members.addAlternative(traitStatement)

    val shapeIdentifier = find(RelativeShapeIdentifierDelta.ShapeIdentifierGrammar)
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val text = find(GenericSmithyDelta.TextGrammar)
    val traitStructure = (text ~ ":" ~~ expression).manySeparatedVertical(",")
    val traitBody: BiGrammar = expression | traitStructure
    val traitValue = "@" ~ shapeIdentifier.as(TraitReference) ~ traitBody.inParenthesis.option asLabelledNode TraitValueShape
    create(Traits, traitValue.manyVertical.as(Traits))
  }

  object Traits extends NodeField

  override def description = "Adds trait statements and trait values"

  override def dependencies = Set(FileWithMembersDelta)

  override def inject(language: Language): Unit = {
    super.inject(language)

    ConstraintSkeleton.hasConstraints.add(language, TraitValueShape, new HasConstraints {
      override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder,
                                      path: NodePath, parentScope: Scope): Unit = {
        RelativeShapeIdentifierDelta.getDeclaration(builder, path(TraitReference).asInstanceOf[NodePath], parentScope, traitType)
      }
    })

    ConstraintSkeleton.hasConstraints.add(language, TraitStatement, new HasConstraints {
      override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder,
                                      path: NodePath, parentScope: Scope): Unit = {
        builder.declare(path.getField(HasNameDelta.Name), parentScope, traitType)
      }
    })
  }
}
