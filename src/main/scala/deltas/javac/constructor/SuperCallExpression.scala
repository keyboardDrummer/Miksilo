package deltas.javac.constructor

import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeShape}
import core.deltas.path.Path
import core.deltas.{Compilation, Contract}
import core.language.Language
import deltas.bytecode.coreInstructions.InvokeSpecialDelta
import deltas.bytecode.coreInstructions.objects.LoadAddressDelta
import deltas.bytecode.types.VoidTypeC
import deltas.javac.classes.MethodQuery
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.classes.skeleton.JavaClassSkeleton._
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import deltas.javac.methods.call.{CallC, CallStaticOrInstanceDelta}
import deltas.javac.statements.StatementSkeleton

object SuperCallExpression extends ExpressionInstance {
  override val key = SuperCall
  val constructorName: String = "<init>"

  override def dependencies: Set[Contract] = Set(CallStaticOrInstanceDelta) ++ super.dependencies

  def superCall(arguments: Seq[Node] = Seq()) = new Node(SuperCall, CallC.CallArguments -> arguments)

  override def getType(expression: Path, compilation: Compilation): Node = VoidTypeC.voidType

  override def toByteCode(call: Path, compilation: Compilation): Seq[Node] = {
    val classCompiler = JavaClassSkeleton.getClassCompiler(compilation)
    transformSuperCall(classCompiler.currentClass, call, compilation)
  }

  def transformSuperCall(program: Node, call: Path, compilation: Compilation): Seq[Node] = {
    transformToByteCode(call, compilation, program.parent.get)
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

  object SuperCall extends NodeShape

  override def description: String = "Enables calling a super constructor."
}
