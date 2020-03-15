package miksilo.modularLanguages.deltas.smithy

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.core.node.{NodeField, NodeShape}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.HasNameDelta
import miksilo.modularLanguages.core.deltas.path.ConstraintBuilderExtension._
import miksilo.modularLanguages.deltas.javac.classes.skeleton.HasConstraintsDelta

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
    builder.declare(path.getField(HasNameDelta.Name), parentScope,
      RelativeShapeIdentifierDelta.shapeType)
  }
}
