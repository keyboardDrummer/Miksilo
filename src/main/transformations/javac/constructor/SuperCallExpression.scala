package transformations.javac.constructor

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{Contract, TransformationState, MetaObject}
import transformations.bytecode.coreInstructions.InvokeSpecialC
import transformations.bytecode.coreInstructions.objects.LoadAddressC
import transformations.javac.classes.{MethodId, ClassC}
import transformations.javac.expressions.{ExpressionC, ExpressionInstance}
import transformations.javac.methods.CallC
import transformations.javac.statements.StatementC
import transformations.types.VoidTypeC

object SuperCallExpression extends ExpressionInstance {
  override val key: AnyRef = SuperCall
  val constructorName: String = "<init>"

  override def dependencies: Set[Contract] = Set(CallC) ++ super.dependencies

  def superCall(arguments: Seq[MetaObject] = Seq()) = new MetaObject(SuperCall) {
    data.put(CallC.CallArguments, arguments)
  }

  override def getType(expression: MetaObject, state: TransformationState): MetaObject = VoidTypeC.voidType

  override def toByteCode(call: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val classCompiler = ClassC.getClassCompiler(state)
    transformSuperCall(classCompiler.currentClass, call, state)
  }

  def transformSuperCall(clazz: MetaObject, call: MetaObject, state: TransformationState): Seq[MetaObject] = {
    transformToByteCode(call, state, ClassC.getParent(clazz).get)
  }

  def transformToByteCode(call: MetaObject, state: TransformationState, className: String): Seq[MetaObject] = {
    val compiler = ClassC.getClassCompiler(state)
    val callArguments = CallC.getCallArguments(call)
    val qualifiedName = compiler.fullyQualify(className)
    val methodRefIndex = compiler.getMethodRefIndex(new MethodId(qualifiedName, constructorName))
    val argumentInstructions = callArguments.flatMap(argument => StatementC.getToInstructions(state)(argument))
    Seq(LoadAddressC.addressLoad(0)) ++ argumentInstructions ++ Seq(InvokeSpecialC.invokeSpecial(methodRefIndex))
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val callArguments = grammars.find(CallC.CallArgumentsGrammar)
    val superCallGrammar = "super" ~> callArguments ^^ parseMap(SuperCall, CallC.CallArguments)
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    expressionGrammar.addOption(superCallGrammar)
  }

  object SuperCall
}
