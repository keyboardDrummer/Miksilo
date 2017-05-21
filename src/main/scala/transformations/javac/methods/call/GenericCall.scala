package transformations.javac.methods.call

import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeLike}
import core.particles.path.Path
import core.particles.{CompilationState, Contract}
import transformations.javac.classes.skeleton.JavaClassSkeleton
import transformations.javac.classes.{ClassCompiler, ClassOrObjectReference, MethodQuery}
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.javac.methods.MemberSelector
import transformations.javac.methods.call.CallC.CallArgumentsGrammar
import transformations.javac.types.MethodTypeC._

object CallC
{
  object CallKey extends Key

  object CallCallee extends Key

  object CallArguments extends Key

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
    val parseCall = calleeGrammar ~ callArguments asNode(CallC.CallKey, CallC.CallCallee, CallC.CallArguments)
    core.addOption(parseCall)
  }

  override val key: Key = CallC.CallKey

  override def getType(call: Path, state: CompilationState): Node = {
    val compiler = JavaClassSkeleton.getClassCompiler(state)
    val methodKey = getMethodKey(call, compiler)
    val methodInfo = compiler.compiler.find(methodKey)
    val returnType = methodInfo._type.returnType
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

    val callArguments = CallC.getCallArguments(call)
    val callTypes: Seq[Node] = callArguments.map(argument => ExpressionSkeleton.getType(compiler.state)(argument))

    val member = MemberSelector.getSelectorMember(callCallee)
    new MethodQuery(kind.info.getQualifiedName, member, callTypes)
  }
}
