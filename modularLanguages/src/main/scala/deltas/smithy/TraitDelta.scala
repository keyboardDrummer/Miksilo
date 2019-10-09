package deltas.smithy

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{NodeField, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.PrimitiveType
import deltas.expression.ExpressionDelta
import deltas.javac.classes.skeleton.HasConstraints
import deltas.json.JsonObjectLiteralDelta
import deltas.{ConstraintSkeleton, FileWithMembersDelta, HasNameDelta}

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
