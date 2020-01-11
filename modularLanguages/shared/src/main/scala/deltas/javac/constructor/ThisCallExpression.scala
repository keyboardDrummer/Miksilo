package deltas.javac.constructor

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{Node, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.types.VoidTypeDelta
import deltas.expression.{ExpressionDelta, ExpressionInstance}
import deltas.javac.methods.call.CallDelta

object ThisCallExpression extends DeltaWithGrammar with ExpressionInstance {

  override def description: String = "Enables calling a different constructor using 'this'"

  override val shape = Shape
  object Shape extends NodeShape

  def thisCall(arguments: Seq[Node]) = new Node(Shape, CallDelta.Arguments -> arguments)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val callArguments = find(CallDelta.CallArgumentsGrammar)
    val thisCallGrammar = "this" ~> callArguments.as(CallDelta.Arguments) asNode Shape
    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar)
    expressionGrammar.addAlternative(thisCallGrammar)
  }

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    builder.typesAreEqual(_type, VoidTypeDelta.constraintType)
  }
}
