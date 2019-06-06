package deltas.smithy

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{GrammarKey, NodeField, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{PrimitiveType, Type}
import deltas.javac.classes.skeleton.HasConstraintsDelta

object RelativeShapeIdentifierDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object ShapeIdentifierGrammar extends GrammarKey
  object Shape extends NodeShape
  object Value extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val relativeShapeId = identifier.as(Value) ~ ("$" ~ identifier).option asLabelledNode Shape
    create(ShapeIdentifierGrammar, relativeShapeId)
  }

  val shapeType = PrimitiveType("shape")

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder,
                                  path: NodePath, parentScope: Scope): Unit = {
    getConstraints(builder, path, parentScope, shapeType)
    // TODO add support for accessing aggregate shape members, the $
  }

  def getConstraints(builder: ConstraintBuilder, path: NodePath, parentScope: Scope, _type: Type) = {
    builder.resolveToType(path.getSourceElement(Value), parentScope, _type)
  }

  override def dependencies = Set.empty

  override def shape = Shape

  override def description = "Adds the relative shape identifiers"
}
