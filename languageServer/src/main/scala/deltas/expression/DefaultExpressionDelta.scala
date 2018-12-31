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
import deltas.expressions.ExpressionDelta
import deltas.javac.expressions.ExpressionInstance

object DefaultExpressionDelta extends DeltaWithGrammar with ExpressionInstance {

  override def description: String = "Adds a default case to parsing an expression"

  object Shape extends NodeShape
  val value = Shape.create()

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
  }

  override def getType(expression: NodePath, compilation: Compilation): Node = throw new Exception("will not implement. Remove method from base")

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val grammar = find(ExpressionDelta.FirstPrecedenceGrammar)
    grammar.inner = new WithDefault(grammar.inner, value)
  }

  override def shape: NodeShape = Shape
}
