package deltas.smithy

import core.bigrammar.grammars.Keyword
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{NodeField, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.{ConstraintSkeleton, HasNameDelta}
import core.deltas.path.ConstraintBuilderExtension._
import deltas.javac.classes.skeleton.HasConstraintsDelta

object SmithyListDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object Shape extends NodeShape
  object ElementShape extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val name = find(HasNameDelta.Name)
    val traits = find(TraitDelta.Traits)
    val shapeIdentifier = find(RelativeShapeIdentifierDelta.ShapeIdentifierGrammar)
    val listBody = name ~ (traits ~ "member" ~ ":" ~~ shapeIdentifier.as(ElementShape)).inBraces
    val grammar = Keyword("list", reserved = false) ~~ listBody asNode Shape
    find(ShapeStatementDelta.ShapeBody).addAlternative(grammar)
  }

  override def description = "Adds the list statement"

  override def dependencies = Set.empty

  override def shape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder,
                                  path: NodePath, parentScope: Scope): Unit = {
    builder.declare(path.getField(HasNameDelta.Name), parentScope,
      RelativeShapeIdentifierDelta.shapeType)

    ConstraintSkeleton.constraints(compilation, builder, path(ElementShape).asInstanceOf[NodePath], parentScope)
  }
}
