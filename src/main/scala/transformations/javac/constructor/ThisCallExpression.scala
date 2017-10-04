package transformations.javac.constructor

import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeClass}
import core.particles.path.Path
import core.particles.{Contract, Language}
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

  override def getType(expression: Path, state: Language): Node = VoidTypeC.voidType

  override def toByteCode(call: Path, state: Language): Seq[Node] = {
    val classCompiler = JavaClassSkeleton.getClassCompiler(state)
    transformThisCall(classCompiler.currentClass, call, state)
  }

  def transformThisCall(clazz: Node, call: Path, state: Language): Seq[Node] = {
    SuperCallExpression.transformToByteCode(call, state, clazz.name)
  }

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val callArguments = grammars.find(CallC.CallArgumentsGrammar)
    val thisCallGrammar = "this" ~> callArguments asNode(ThisCall, CallC.CallArguments)
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    expressionGrammar.addOption(thisCallGrammar)
  }

  override def description: String = "Enables calling a different constructor using 'this'"
}
