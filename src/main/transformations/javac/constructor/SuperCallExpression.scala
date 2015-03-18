package transformations.javac.constructor

import core.particles.grammars.GrammarCatalogue
import core.particles.{Path$, CompilationState, Contract, MetaObject}
import transformations.bytecode.coreInstructions.InvokeSpecialC
import transformations.bytecode.coreInstructions.objects.LoadAddressC
import transformations.javac.classes.{JavaClassSkeleton, MethodId}
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.javac.methods.CallC
import transformations.javac.statements.StatementSkeleton
import transformations.types.VoidTypeC

object SuperCallExpression extends ExpressionInstance {
  override val key: AnyRef = SuperCall
  val constructorName: String = "<init>"

  override def dependencies: Set[Contract] = Set(CallC) ++ super.dependencies

  def superCall(arguments: Seq[MetaObject] = Seq()) = new MetaObject(SuperCall, CallC.CallArguments -> arguments)

  override def getType(expression: Path, state: CompilationState): MetaObject = VoidTypeC.voidType

  override def toByteCode(call: Path, state: CompilationState): Seq[MetaObject] = {
    val classCompiler = JavaClassSkeleton.getClassCompiler(state)
    transformSuperCall(classCompiler.currentClass, call, state)
  }

  def transformSuperCall(clazz: MetaObject, call: Path, state: CompilationState): Seq[MetaObject] = {
    transformToByteCode(call, state, JavaClassSkeleton.getParent(clazz).get)
  }

  def transformToByteCode(call: Path, state: CompilationState, className: String): Seq[MetaObject] = {
    val compiler = JavaClassSkeleton.getClassCompiler(state)
    val callArguments = CallC.getCallArguments(call)
    val qualifiedName = compiler.fullyQualify(className)
    val methodRefIndex = compiler.getMethodRefIndex(new MethodId(qualifiedName, constructorName))
    val argumentInstructions = callArguments.flatMap(argument => StatementSkeleton.getToInstructions(state)(argument))
    Seq(LoadAddressC.addressLoad(0)) ++ argumentInstructions ++ Seq(InvokeSpecialC.invokeSpecial(methodRefIndex))
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val callArguments = grammars.find(CallC.CallArgumentsGrammar)
    val superCallGrammar = "super" ~> callArguments ^^ parseMap(SuperCall, CallC.CallArguments)
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    expressionGrammar.addOption(superCallGrammar)
  }

  object SuperCall

  override def description: String = "Enables calling a super constructor."
}
