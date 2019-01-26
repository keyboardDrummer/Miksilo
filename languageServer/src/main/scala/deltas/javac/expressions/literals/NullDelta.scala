package deltas.javac.expressions.literals

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{Node, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.coreInstructions.objects.PushNullDelta
import deltas.expression.{ExpressionDelta, ExpressionInstance}
import deltas.javac.expressions.ConvertsToByteCodeDelta

object NullDelta extends DeltaWithGrammar with ExpressionInstance with ConvertsToByteCodeDelta {

  val _null = new Node(Shape)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar)
    val parseNull = "null" ~> value(_null)
    expressionGrammar.inner = expressionGrammar.inner | parseNull
  }

  override def dependencies: Set[Contract] = Set(ExpressionDelta, PushNullDelta)

  object Shape extends NodeShape

  override val shape = Shape

  override def toByteCode(expression: NodePath, compilation: Compilation): Seq[Node] = {
    Seq(PushNullDelta.pushNull)
  }

  override def description: String = "Adds the usage of 'null'"

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = ???
}
