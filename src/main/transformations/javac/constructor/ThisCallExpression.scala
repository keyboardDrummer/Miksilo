package transformations.javac.constructor

import core.particles.grammars.GrammarCatalogue
import core.particles.{Path$, CompilationState, Contract, MetaObject}
import transformations.javac.classes.JavaClassSkeleton
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.javac.methods.CallC
import transformations.types.VoidTypeC

object ThisCallExpression extends ExpressionInstance {
  override val key: AnyRef = ThisCall
  object ThisCall

  def thisCall(arguments: Seq[MetaObject]) = new MetaObject(ThisCall, CallC.CallArguments -> arguments)

  override def dependencies: Set[Contract] = Set(SuperCallExpression) ++ super.dependencies

  override def getType(expression: Path, state: CompilationState): MetaObject = VoidTypeC.voidType

  override def toByteCode(call: Path, state: CompilationState): Seq[MetaObject] = {
    val classCompiler = JavaClassSkeleton.getClassCompiler(state)
    transformThisCall(classCompiler.currentClass, call, state)
  }

  def transformThisCall(clazz: MetaObject, call: Path, state: CompilationState): Seq[MetaObject] = {
    SuperCallExpression.transformToByteCode(call, state, JavaClassSkeleton.getClassName(clazz))
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val callArguments = grammars.find(CallC.CallArgumentsGrammar)
    val thisCallGrammar = "this" ~> callArguments ^^ parseMap(ThisCall, CallC.CallArguments)
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    expressionGrammar.addOption(thisCallGrammar)
  }

  override def description: String = "Enables calling a different constructor using 'this'"
}
