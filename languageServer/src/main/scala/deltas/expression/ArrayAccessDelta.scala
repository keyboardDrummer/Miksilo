package deltas.expression

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{NodeField, NodeLike, NodeShape, NodeWrapper}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{Type, TypeApplication}
import deltas.bytecode.types.ArrayTypeDelta

object ArrayAccessDelta extends DeltaWithGrammar with ExpressionInstance {

  object Shape extends NodeShape
  object Target extends NodeField
  object Index extends NodeField

  implicit class ArrayAccess[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def target: T = node(Target).asInstanceOf[T]
    def index: Int = node.getValue(Index).asInstanceOf[Int]
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val grammar = expression.as(Target) ~ "[" ~ expression.as(Index) ~ "]" asLabelledNode Shape
    expression.addAlternative(grammar)
  }

  override def description = "Enables accessing values from arrays"

  override def dependencies = Set(ExpressionDelta)

  override def shape = Shape

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, elementType: Type, parentScope: Scope): Unit = {
    val arrayAccess: ArrayAccess[NodePath] = expression
    val arrayType = ExpressionDelta.getType(compilation, builder, arrayAccess.target, parentScope)
    builder.typesAreEqual(arrayType, TypeApplication(ArrayTypeDelta.arrayTypeConstructor, Seq(elementType), expression))
  }
}
