package deltas.javac.constructor

import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeShape}
import core.deltas.path.NodePath
import core.deltas.{Compilation, Contract}
import core.language.Language
import deltas.bytecode.types.VoidTypeDelta
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.classes.skeleton.JavaClassSkeleton._
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import deltas.javac.methods.call.CallDelta

object ThisCallExpression extends ExpressionInstance {
  override val key = ThisCall
  object ThisCall extends NodeShape

  def thisCall(arguments: Seq[Node]) = new Node(ThisCall, CallDelta.CallArguments -> arguments)

  override def dependencies: Set[Contract] = Set(SuperCallExpression) ++ super.dependencies

  override def getType(expression: NodePath, compilation: Compilation): Node = VoidTypeDelta.voidType

  override def toByteCode(call: NodePath, compilation: Compilation): Seq[Node] = {
    val classCompiler = JavaClassSkeleton.getClassCompiler(compilation)
    transformThisCall(classCompiler.currentClass, call, compilation)
  }

  def transformThisCall(program: Node, call: NodePath, compilation: Compilation): Seq[Node] = {
    SuperCallExpression.transformToByteCode(call, compilation, program.name)
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val callArguments = find(CallDelta.CallArgumentsGrammar)
    val thisCallGrammar = "this" ~> callArguments.as(CallDelta.CallArguments) asNode ThisCall
    val expressionGrammar = find(ExpressionSkeleton.ExpressionGrammar)
    expressionGrammar.addOption(thisCallGrammar)
  }

  override def description: String = "Enables calling a different constructor using 'this'"
}
