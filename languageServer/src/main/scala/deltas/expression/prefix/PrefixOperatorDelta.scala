package deltas.expression.prefix

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.node.{NodeField, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.expression.{ExpressionDelta, ExpressionInstance}

trait PrefixOperatorDelta extends DeltaWithGrammar with ExpressionInstance {

  def keyword: String

  override val shape = Shape

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val expressionCore = find(ExpressionDelta.LastPrecedenceGrammar)
    val prefixOperator = keyword ~> expressionCore.as(Target) asNode Shape
    expressionCore.addAlternative(prefixOperator)
  }

  object Shape extends NodeShape
  object Target extends NodeField

  override def description: String = s"Adds the prefix $keyword operator."

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    ExpressionDelta.addConstraints(compilation, builder, expression(Target).asInstanceOf[NodePath], _type, parentScope)
  }
}
