package transformations.javac.constructor

import core.transformation.{Contract, TransformationState, MetaObject}
import core.transformation.grammars.GrammarCatalogue
import transformations.javac.classes.ClassC
import transformations.javac.expressions.{ExpressionInstance, ExpressionC}
import transformations.javac.methods.CallC
import transformations.types.VoidTypeC

object ThisCallExpression extends ExpressionInstance {
  override val key: AnyRef = ThisCall
  object ThisCall

  def thisCall(arguments: Seq[MetaObject]) = new MetaObject(ThisCall) {
    data.put(CallC.CallArguments, arguments)
  }

  override def dependencies: Set[Contract] = Set(SuperCallExpression) ++ super.dependencies

  override def getType(expression: MetaObject, state: TransformationState): MetaObject = VoidTypeC.voidType

  override def toByteCode(call: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val classCompiler = ClassC.getClassCompiler(state)
    transformThisCall(classCompiler.currentClass, call, state)
  }

  def transformThisCall(clazz: MetaObject, call: MetaObject, state: TransformationState): Seq[MetaObject] = {
    SuperCallExpression.transformToByteCode(call, state, ClassC.getClassName(clazz))
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val callArguments = grammars.find(CallC.CallArgumentsGrammar)
    val thisCallGrammar = "this" ~> callArguments ^^ parseMap(ThisCall, CallC.CallArguments)
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    expressionGrammar.addOption(thisCallGrammar)
  }
}
