package transformations.javac.constructor

import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass}
import core.particles.path.Path
import core.particles.{Compilation, Contract, Language}
import transformations.bytecode.types.VoidTypeC
import transformations.javac.classes.skeleton.JavaClassSkeleton
import transformations.javac.classes.skeleton.JavaClassSkeleton._
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.javac.methods.call.CallC

object ThisCallExpression extends ExpressionInstance {
  override val key = ThisCall
  object ThisCall extends NodeClass

  def thisCall(arguments: Seq[Node]) = new Node(ThisCall, CallC.CallArguments -> arguments)

  override def dependencies: Set[Contract] = Set(SuperCallExpression) ++ super.dependencies

  override def getType(expression: Path, compilation: Compilation): Node = VoidTypeC.voidType

  override def toByteCode(call: Path, compilation: Compilation): Seq[Node] = {
    val classCompiler = JavaClassSkeleton.getClassCompiler(compilation)
    transformThisCall(classCompiler.currentClass, call, compilation)
  }

  def transformThisCall(clazz: Node, call: Path, compilation: Compilation): Seq[Node] = {
    SuperCallExpression.transformToByteCode(call, compilation, clazz.name)
  }

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val callArguments = grammars.find(CallC.CallArgumentsGrammar)
    val thisCallGrammar = "this" ~> callArguments.as(CallC.CallArguments) asNode ThisCall
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    expressionGrammar.addOption(thisCallGrammar)
  }

  override def description: String = "Enables calling a different constructor using 'this'"
}
