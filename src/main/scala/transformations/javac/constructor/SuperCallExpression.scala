package transformations.javac.constructor

import core.particles.grammars.LanguageGrammars
import core.particles.node.{Node, NodeClass}
import core.particles.path.Path
import core.particles.{Compilation, Contract, Language}
import transformations.bytecode.coreInstructions.InvokeSpecialDelta
import transformations.bytecode.coreInstructions.objects.LoadAddressDelta
import transformations.bytecode.types.VoidTypeC
import transformations.javac.classes.MethodQuery
import transformations.javac.classes.skeleton.JavaClassSkeleton
import transformations.javac.classes.skeleton.JavaClassSkeleton._
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.javac.methods.call.{CallC, CallStaticOrInstanceC}
import transformations.javac.statements.StatementSkeleton

object SuperCallExpression extends ExpressionInstance {
  override val key = SuperCall
  val constructorName: String = "<init>"

  override def dependencies: Set[Contract] = Set(CallStaticOrInstanceC) ++ super.dependencies

  def superCall(arguments: Seq[Node] = Seq()) = new Node(SuperCall, CallC.CallArguments -> arguments)

  override def getType(expression: Path, compilation: Compilation): Node = VoidTypeC.voidType

  override def toByteCode(call: Path, compilation: Compilation): Seq[Node] = {
    val classCompiler = JavaClassSkeleton.getClassCompiler(compilation)
    transformSuperCall(classCompiler.currentClass, call, compilation)
  }

  def transformSuperCall(clazz: Node, call: Path, compilation: Compilation): Seq[Node] = {
    transformToByteCode(call, compilation, clazz.parent.get)
  }

  def transformToByteCode(call: Path, compilation: Compilation, className: String): Seq[Node] = {
    val compiler = JavaClassSkeleton.getClassCompiler(compilation)
    val callArguments = CallC.getCallArguments(call)
    val callTypes = callArguments.map(argument => ExpressionSkeleton.getType(compilation)(argument))
    val qualifiedName = compiler.fullyQualify(className)
    val methodRefIndex = compiler.getMethodRefIndex(MethodQuery(qualifiedName, constructorName, callTypes))
    val argumentInstructions = callArguments.flatMap(argument => StatementSkeleton.getToInstructions(compilation)(argument))
    Seq(LoadAddressDelta.addressLoad(0)) ++ argumentInstructions ++ Seq(InvokeSpecialDelta.invokeSpecial(methodRefIndex))
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val callArguments = find(CallC.CallArgumentsGrammar)
    val superCallGrammar = "super" ~> callArguments.as(CallC.CallArguments) asNode SuperCall
    val expressionGrammar = find(ExpressionSkeleton.ExpressionGrammar)
    expressionGrammar.addOption(superCallGrammar)
  }

  object SuperCall extends NodeClass

  override def description: String = "Enables calling a super constructor."
}
