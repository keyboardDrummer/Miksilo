package deltas.javac.constructor

import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeShape}
import core.deltas.path.Path
import core.deltas.{Compilation, Contract}
import core.language.Language
import deltas.bytecode.types.VoidTypeC
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.classes.skeleton.JavaClassSkeleton._
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import deltas.javac.methods.call.CallC

object ThisCallExpression extends ExpressionInstance {
  override val key = ThisCall
  object ThisCall extends NodeShape

  def thisCall(arguments: Seq[Node]) = new Node(ThisCall, CallC.CallArguments -> arguments)

  override def dependencies: Set[Contract] = Set(SuperCallExpression) ++ super.dependencies

  override def getType(expression: Path, compilation: Compilation): Node = VoidTypeC.voidType

  override def toByteCode(call: Path, compilation: Compilation): Seq[Node] = {
    val classCompiler = JavaClassSkeleton.getClassCompiler(compilation)
    transformThisCall(classCompiler.currentClass, call, compilation)
  }

  def transformThisCall(program: Node, call: Path, compilation: Compilation): Seq[Node] = {
    SuperCallExpression.transformToByteCode(call, compilation, program.name)
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val callArguments = find(CallC.CallArgumentsGrammar)
    val thisCallGrammar = "this" ~> callArguments.as(CallC.CallArguments) asNode ThisCall
    val expressionGrammar = find(ExpressionSkeleton.ExpressionGrammar)
    expressionGrammar.addOption(thisCallGrammar)
  }

  override def description: String = "Enables calling a different constructor using 'this'"
}
