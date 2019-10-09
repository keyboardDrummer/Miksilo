package deltas.expression

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{NodeField, NodeShape, _}
import core.language.{Compilation, Language}
import core.parsers.editorParsers.History
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.json.JsonObjectLiteralDelta

object ArrayLiteralDelta extends DeltaWithGrammar with ExpressionInstance {

  override def description: String = "Adds the array literal to expressions"

  def create(elements: Seq[Node]) = Shape.create(Members -> elements)

  implicit class ArrayLiteral[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def members: Seq[T] = node(Members).asInstanceOf[Seq[T]]
  }

  override def transformGrammars(_grammars: LanguageGrammars, language: Language): Unit = {
    val grammars = _grammars
    import _grammars._

    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val array = Delimiter("[", History.missingInputPenalty * 2) ~ expression.manySeparated(",").as(Members) ~ "]"
    val grammar = array.asLabelledNode(Shape)
    expression.addAlternative(grammar)
  }

  object Members extends NodeField
  object Shape extends NodeShape
  override def shape: NodeShape = Shape

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {

  }
}
