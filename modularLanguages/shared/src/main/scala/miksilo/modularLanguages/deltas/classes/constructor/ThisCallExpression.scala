package miksilo.modularLanguages.deltas.javac.constructor

import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node.{Node, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.Type
import miksilo.modularLanguages.deltas.bytecode.types.VoidTypeDelta
import miksilo.modularLanguages.deltas.expression.{ExpressionDelta, ExpressionInstance}
import miksilo.modularLanguages.deltas.javac.methods.call.CallDelta

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
