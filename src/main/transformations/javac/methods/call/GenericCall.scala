package transformations.javac.methods.call

import core.particles.grammars.GrammarCatalogue
import core.particles.{Contract, CompilationState}
import core.particles.node.{NodeLike, Node}
import core.particles.path.Path
import transformations.bytecode.constants.MethodDescriptorConstant
import transformations.javac.classes.{JavaClassSkeleton, MethodId, ClassOrObjectReference, ClassCompiler}
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.javac.methods.MemberSelector
import transformations.javac.methods.call.CallC.CallArgumentsGrammar

object CallC
{
  object CallKey

  object CallCallee

  object CallArguments

  object CallArgumentsGrammar

  def getCallCallee[T <: NodeLike](call: T) = call(CallC.CallCallee).asInstanceOf[T]

  def getCallArguments[T <: NodeLike](call: T) = call(CallC.CallArguments).asInstanceOf[Seq[T]]

  def call(callee: Any, arguments: Any): Node =
    call(callee.asInstanceOf[Node], arguments.asInstanceOf[Seq[Node]])

  def call(callee: Node, arguments: Seq[Node] = Seq()) = {
    new Node(CallC.CallKey, CallC.CallCallee -> callee, CallC.CallArguments -> arguments)
  }
}

trait GenericCall extends ExpressionInstance {

  override def dependencies: Set[Contract] = Set(MemberSelector)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val core = grammars.find(ExpressionSkeleton.CoreGrammar)
    val expression = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val selectorGrammar = grammars.find(MemberSelector.SelectGrammar)
    val calleeGrammar = grammars.create(CallC.CallCallee, selectorGrammar)
    val callArguments = grammars.create(CallArgumentsGrammar, "(" ~> expression.manySeparated(",") <~ ")")
    val parseCall = calleeGrammar ~ callArguments ^^ parseMap(CallC.CallKey, CallC.CallCallee, CallC.CallArguments)
    core.addOption(parseCall)
  }

  override val key: AnyRef = CallC.CallKey

  override def getType(call: Path, state: CompilationState): Node = {
    val compiler = JavaClassSkeleton.getClassCompiler(state)
    val methodKey = getMethodKey(call, compiler)
    val methodInfo = compiler.compiler.find(methodKey)
    val returnType = MethodDescriptorConstant.getMethodDescriptorReturnType(methodInfo.descriptor)
    returnType
  }

  def getGenericCallInstructions(call: Path, state: CompilationState, calleeInstructions: Seq[Node], invokeInstructions: Seq[Node]): Seq[Node] = {
    val expressionToInstruction = ExpressionSkeleton.getToInstructions(state)
    val callArguments = CallC.getCallArguments(call)
    val argumentInstructions = callArguments.flatMap(argument => expressionToInstruction(argument))
    calleeInstructions ++ argumentInstructions ++ invokeInstructions
  }

  def getMethodKey(call: Path, compiler: ClassCompiler) = {
    val callCallee = CallC.getCallCallee(call)
    val objectExpression = MemberSelector.getSelectorObject(callCallee)
    val kind = MemberSelector.getReferenceKind(compiler, objectExpression).asInstanceOf[ClassOrObjectReference]

    val member = MemberSelector.getSelectorMember(callCallee)
    new MethodId(kind.info.getQualifiedName, member)
  }

  override def description: String = "Enables calling static and virtual methods."
}
