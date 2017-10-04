package transformations.javac.constructor

import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeClass}
import core.particles.path.Path
import core.particles.{Contract, Language}
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

  override def getType(expression: Path, state: Language): Node = VoidTypeC.voidType

  override def toByteCode(call: Path, state: Language): Seq[Node] = {
    val classCompiler = JavaClassSkeleton.getClassCompiler(state)
    transformSuperCall(classCompiler.currentClass, call, state)
  }

  def transformSuperCall(clazz: Node, call: Path, state: Language): Seq[Node] = {
    transformToByteCode(call, state, clazz.parent.get)
  }

  def transformToByteCode(call: Path, state: Language, className: String): Seq[Node] = {
    val compiler = JavaClassSkeleton.getClassCompiler(state)
    val callArguments = CallC.getCallArguments(call)
    val callTypes = callArguments.map(argument => ExpressionSkeleton.getType(state)(argument))
    val qualifiedName = compiler.fullyQualify(className)
    val methodRefIndex = compiler.getMethodRefIndex(new MethodQuery(qualifiedName, constructorName, callTypes))
    val argumentInstructions = callArguments.flatMap(argument => StatementSkeleton.getToInstructions(state)(argument))
    Seq(LoadAddressDelta.addressLoad(0)) ++ argumentInstructions ++ Seq(InvokeSpecialDelta.invokeSpecial(methodRefIndex))
  }

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val callArguments = grammars.find(CallC.CallArgumentsGrammar)
    val superCallGrammar = "super" ~> callArguments asNode(SuperCall, CallC.CallArguments)
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    expressionGrammar.addOption(superCallGrammar)
  }

  object SuperCall extends NodeClass

  override def description: String = "Enables calling a super constructor."
}
