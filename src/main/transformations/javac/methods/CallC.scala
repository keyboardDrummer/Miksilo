package transformations.javac.methods

import core.particles._
import core.particles.grammars.GrammarCatalogue
import transformations.bytecode.constants.MethodDescriptorConstant
import transformations.bytecode.coreInstructions.{InvokeStaticC, InvokeVirtualC}
import transformations.javac.classes._
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}

object CallC extends ExpressionInstance {

  def getCallCallee(call: MetaObject) = call(CallCallee).asInstanceOf[MetaObject]

  def getCallArguments(call: MetaObject) = call(CallArguments).asInstanceOf[Seq[MetaObject]]

  override def dependencies: Set[Contract] = Set(SelectorC, InvokeStaticC, InvokeVirtualC)

  object CallArgumentsGrammar
  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val core = grammars.find(ExpressionSkeleton.CoreGrammar)
    val expression = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val callArguments = grammars.create(CallArgumentsGrammar, "(" ~> expression.manySeparated(",") <~ ")")
    val parseCall = expression ~ callArguments ^^ parseMap(CallKey, CallCallee, CallArguments)
    core.addOption(parseCall)
  }

  def call(callee: Any, arguments: Any): MetaObject =
    call(callee.asInstanceOf[MetaObject], arguments.asInstanceOf[Seq[MetaObject]])

  def call(callee: MetaObject, arguments: Seq[MetaObject] = Seq()) = {
    new MetaObject(CallKey) {
      data.put(CallCallee, callee)
      data.put(CallArguments, arguments)
    }
  }

  object CallKey

  object CallCallee

  object CallArguments

  override val key: AnyRef = CallKey

  override def getType(call: MetaObject, state: CompilationState): MetaObject = {
    val compiler = JavaClassSkeleton.getClassCompiler(state)
    val methodKey = getMethodKey(call, compiler)
    val methodInfo = compiler.compiler.find(methodKey)
    val returnType = MethodDescriptorConstant.getMethodDescriptorReturnType(methodInfo.descriptor)
    returnType
  }

  override def toByteCode(call: MetaObject, state: CompilationState): Seq[MetaObject] = {
    val compiler = JavaClassSkeleton.getClassCompiler(state)

    val callCallee = getCallCallee(call)
    val objectExpression = SelectorC.getSelectorObject(callCallee)
    val methodKey: MethodId = getMethodKey(call, compiler)
    val methodInfo = compiler.compiler.find(methodKey)
    val staticCall = methodInfo._static
    val expressionToInstruction = ExpressionSkeleton.getToInstructions(compiler.state)
    val calleeInstructions =
      if (!staticCall) expressionToInstruction(objectExpression)
      else Seq[MetaObject]()
    val callArguments = getCallArguments(call)
    val argumentInstructions = callArguments.flatMap(argument => expressionToInstruction(argument))
    val methodRefIndex = compiler.getMethodRefIndex(methodKey)
    val invokeInstructions = Seq(if (staticCall)
      InvokeStaticC.invokeStatic(methodRefIndex)
    else
      InvokeVirtualC.invokeVirtual(methodRefIndex))
    calleeInstructions ++ argumentInstructions ++ invokeInstructions
  }

  def getMethodKey(call: MetaObject, compiler: ClassCompiler) = {
    val callCallee = getCallCallee(call)
    val objectExpression = SelectorC.getSelectorObject(callCallee)
    val kind = SelectorC.getReferenceKind(compiler, objectExpression).asInstanceOf[ClassOrObjectReference]

    val member = SelectorC.getSelectorMember(callCallee)
    new MethodId(kind.info.getQualifiedName, member)
  }

  override def description: String = "Enables calling static and virtual methods."
}
