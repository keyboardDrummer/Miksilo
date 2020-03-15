package miksilo.modularLanguages.deltas.smithy

import miksilo.modularLanguages.core.bigrammar.grammars.Keyword
import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node.{NodeField, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.{ConstraintSkeleton, HasNameDelta}
import miksilo.modularLanguages.core.deltas.path.ConstraintBuilderExtension._
import miksilo.modularLanguages.deltas.javac.classes.skeleton.HasConstraintsDelta

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
