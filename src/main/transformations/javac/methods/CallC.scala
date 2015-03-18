package transformations.javac.methods

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, MetaLike}
import core.particles.path.Path
import transformations.bytecode.constants.MethodDescriptorConstant
import transformations.bytecode.coreInstructions.{InvokeStaticC, InvokeVirtualC}
import transformations.javac.classes._
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}

object CallC extends ExpressionInstance {

  def getCallCallee[T <: MetaLike](call: T) = call(CallCallee).asInstanceOf[T]

  def getCallArguments[T <: MetaLike](call: T) = call(CallArguments).asInstanceOf[Seq[T]]

  override def dependencies: Set[Contract] = Set(MemberSelector, InvokeStaticC, InvokeVirtualC)

  object CallArgumentsGrammar
  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val core = grammars.find(ExpressionSkeleton.CoreGrammar)
    val expression = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val selectorGrammar = grammars.find(MemberSelector.SelectGrammar)
    val calleeGrammar = grammars.create(CallCallee, selectorGrammar)
    val callArguments = grammars.create(CallArgumentsGrammar, "(" ~> expression.manySeparated(",") <~ ")")
    val parseCall = calleeGrammar ~ callArguments ^^ parseMap(CallKey, CallCallee, CallArguments)
    core.addOption(parseCall)
  }

  def call(callee: Any, arguments: Any): Node =
    call(callee.asInstanceOf[Node], arguments.asInstanceOf[Seq[Node]])

  def call(callee: Node, arguments: Seq[Node] = Seq()) = {
    new Node(CallKey, CallCallee -> callee, CallArguments -> arguments)
  }

  object CallKey

  object CallCallee

  object CallArguments

  override val key: AnyRef = CallKey

  override def getType(call: Path, state: CompilationState): Node = {
    val compiler = JavaClassSkeleton.getClassCompiler(state)
    val methodKey = getMethodKey(call, compiler)
    val methodInfo = compiler.compiler.find(methodKey)
    val returnType = MethodDescriptorConstant.getMethodDescriptorReturnType(methodInfo.descriptor)
    returnType
  }

  override def toByteCode(call: Path, state: CompilationState): Seq[Node] = {
    val compiler = JavaClassSkeleton.getClassCompiler(state)

    val callCallee = getCallCallee(call)
    val objectExpression = MemberSelector.getSelectorObject(callCallee)
    val methodKey: MethodId = getMethodKey(call, compiler)
    val methodInfo = compiler.compiler.find(methodKey)
    val staticCall = methodInfo._static
    val expressionToInstruction = ExpressionSkeleton.getToInstructions(compiler.state)
    val calleeInstructions =
      if (!staticCall) expressionToInstruction(objectExpression)
      else Seq[Node]()
    val callArguments = getCallArguments(call)
    val argumentInstructions = callArguments.flatMap(argument => expressionToInstruction(argument))
    val methodRefIndex = compiler.getMethodRefIndex(methodKey)
    val invokeInstructions = Seq(if (staticCall)
      InvokeStaticC.invokeStatic(methodRefIndex)
    else
      InvokeVirtualC.invokeVirtual(methodRefIndex))
    calleeInstructions ++ argumentInstructions ++ invokeInstructions
  }

  def getMethodKey(call: Path, compiler: ClassCompiler) = {
    val callCallee = getCallCallee(call)
    val objectExpression = MemberSelector.getSelectorObject(callCallee)
    val kind = MemberSelector.getReferenceKind(compiler, objectExpression).asInstanceOf[ClassOrObjectReference]

    val member = MemberSelector.getSelectorMember(callCallee)
    new MethodId(kind.info.getQualifiedName, member)
  }

  override def description: String = "Enables calling static and virtual methods."
}
