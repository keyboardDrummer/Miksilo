package deltas.javac.methods.call

import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.deltas.path.Path
import core.deltas.{Compilation, Contract, Language}
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.classes.{ClassCompiler, ClassOrObjectReference, MethodQuery}
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import deltas.javac.methods.MemberSelector
import deltas.javac.methods.call.CallC.CallArgumentsGrammar
import deltas.javac.types.MethodType._

object CallC
{
  object CallKey extends NodeClass

  object CallCallee extends NodeField

  object CallArguments extends NodeField

  object CallArgumentsGrammar extends GrammarKey

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

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val core = find(ExpressionSkeleton.CoreGrammar)
    val expression = find(ExpressionSkeleton.ExpressionGrammar)
    val selectorGrammar = find(MemberSelector.SelectGrammar)
    val calleeGrammar = create(CallC.CallCallee, selectorGrammar)
    val callArguments = create(CallArgumentsGrammar, "(" ~> expression.manySeparated(",") ~< ")")
    val parseCall = calleeGrammar.as(CallC.CallCallee) ~ callArguments.as(CallC.CallArguments) asNode CallC.CallKey
    core.addOption(parseCall)
  }

  override val key = CallC.CallKey

  override def getType(call: Path, compilation: Compilation): Node = {
    val compiler = JavaClassSkeleton.getClassCompiler(compilation)
    val methodKey = getMethodKey(call, compiler)
    val methodInfo = compiler.javaCompiler.find(methodKey)
    val returnType = methodInfo._type.returnType
    returnType
  }

  def getGenericCallInstructions(call: Path, compilation: Compilation, calleeInstructions: Seq[Node], invokeInstructions: Seq[Node]): Seq[Node] = {
    val expressionToInstruction = ExpressionSkeleton.getToInstructions(compilation)
    val callArguments = CallC.getCallArguments(call)
    val argumentInstructions = callArguments.flatMap(argument => expressionToInstruction(argument))
    calleeInstructions ++ argumentInstructions ++ invokeInstructions
  }

  def getMethodKey(call: Path, compiler: ClassCompiler) = {
    val callCallee = CallC.getCallCallee(call)
    val objectExpression = MemberSelector.getSelectorObject(callCallee)
    val kind = MemberSelector.getReferenceKind(compiler, objectExpression).asInstanceOf[ClassOrObjectReference]

    val callArguments = CallC.getCallArguments(call)
    val callTypes: Seq[Node] = callArguments.map(argument => ExpressionSkeleton.getType(compiler.compilation)(argument))

    val member = MemberSelector.getSelectorMember(callCallee)
    new MethodQuery(kind.info.getQualifiedName, member, callTypes)
  }
}
