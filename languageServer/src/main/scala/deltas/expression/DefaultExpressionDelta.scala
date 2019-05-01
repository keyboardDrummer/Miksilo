package deltas.expression

import core.bigrammar.grammars.WithDefault
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{Node, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type

object DefaultExpressionDelta {
  object Shape extends NodeShape
}

case class DefaultExpressionDelta(defaultName: String = "expression") extends DeltaWithGrammar with ExpressionInstance {

  override def description: String = "Adds a default case to parsing an expression"

  val value = DefaultExpressionDelta.Shape.create()

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val grammar = find(ExpressionDelta.FirstPrecedenceGrammar)
    grammar.inner = new WithDefault(grammar.inner, value, defaultName)
  }

  override def shape: NodeShape = DefaultExpressionDelta.Shape
}
