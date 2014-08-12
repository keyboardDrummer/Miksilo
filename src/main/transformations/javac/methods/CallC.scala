package transformations.javac.methods

import core.grammar.{Grammar, seqr}
import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.coreInstructions.{InvokeStaticC, InvokeVirtualC}
import transformations.javac.classes._
import transformations.javac.expressions.ExpressionC

object CallC extends GrammarTransformation {

  override def inject(state: TransformationState): Unit = {
    ExpressionC.getGetTypeRegistry(state).put(CallKey, (call: MetaObject) => {
      val compiler = ClassC.getClassCompiler(state)
      val methodKey = getMethodKey(call, compiler)
      val methodInfo = compiler.compiler.find(methodKey)
      val returnType = ByteCodeSkeleton.getMethodDescriptorReturnType(methodInfo.descriptor)
      returnType
    })
    ExpressionC.getExpressionToLines(state).put(CallKey, (call: MetaObject) => {
      val compiler = ClassC.getClassCompiler(state)
      callToLines(call, compiler)
    })
  }

  def callToLines(call: MetaObject, compiler: ClassCompiler): Seq[MetaObject] = {
    val callCallee = getCallCallee(call)
    val objectExpression = SelectorC.getSelectorObject(callCallee)
    val methodKey: MethodId = getMethodKey(call, compiler)
    val methodInfo = compiler.compiler.find(methodKey)
    val staticCall = methodInfo._static
    val expressionToInstruction = ExpressionC.getToInstructions(compiler.transformationState)
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
    val kind = compiler.getReferenceKind(objectExpression).asInstanceOf[ClassOrObjectReference]

    val member = SelectorC.getSelectorMember(callCallee)
    new MethodId(kind.info.getQualifiedName, member)
  }

  def getCallCallee(call: MetaObject) = call(CallCallee).asInstanceOf[MetaObject]

  def getCallArguments(call: MetaObject) = call(CallArguments).asInstanceOf[Seq[MetaObject]]

  override def dependencies: Set[Contract] = Set(SelectorC, InvokeStaticC, InvokeVirtualC)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val core = grammars.find(ExpressionC.CoreGrammar)
    val expression = grammars.find(ExpressionC.ExpressionGrammar)
    val callArguments: Grammar = "(" ~> expression.manySeparated(",") <~ ")"
    val parseCall = expression ~ callArguments ^^ { case callee seqr arguments => call(callee, arguments)}
    core.inner = core.inner | parseCall
  }

  def call(callee: Any, arguments: Any): MetaObject = call(callee.asInstanceOf[MetaObject], arguments.asInstanceOf[Seq[MetaObject]])

  def call(callee: MetaObject, arguments: Seq[MetaObject] = Seq()) = {
    new MetaObject(CallKey) {
      data.put(CallCallee, callee)
      data.put(CallArguments, arguments)
    }
  }

  object CallKey

  object CallCallee

  object CallArguments

}
