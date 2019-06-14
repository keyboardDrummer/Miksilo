package deltas.javac.expressions.literals

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{Node, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.expression.{ExpressionDelta, ExpressionInstance}

object NullDelta extends DeltaWithGrammar with ExpressionInstance {

  val _null = new Node(Shape)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar)
    val parseNull = "null" ~> value(_null)
    expressionGrammar.addAlternative(parseNull)
  }

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  object Shape extends NodeShape

  override val shape = Shape

  override def description: String = "Adds the usage of 'null'"

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = ???
}
