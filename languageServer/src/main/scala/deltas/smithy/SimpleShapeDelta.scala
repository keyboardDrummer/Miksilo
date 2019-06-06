package deltas.smithy

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.{Compilation, Language}
import core.language.node.{NodeField, NodeShape}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.HasNameDelta
import deltas.javac.classes.skeleton.HasConstraintsDelta

object SimpleShapeDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object Shape extends NodeShape
  object Type extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val values: BiGrammar = "blob" | "boolean" | "string" | "byte" | "short" | "integer" | "long" | "float" |
      "double" | "bigInteger" | "bigDecimal" | "timestamp"

    val name = find(HasNameDelta.Name)
    val grammar = values.as(Type) ~~ name asLabelledNode Shape
    find(ShapeStatementDelta.ShapeBody).addAlternative(grammar)
 }

  override def description = "Adds simple shapes"

  override def dependencies = Set(ShapeStatementDelta)

  override def shape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    builder.declareSourceElement(path.getSourceElement(HasNameDelta.Name), parentScope,
      Some(RelativeShapeIdentifierDelta.shapeType))
  }
}
