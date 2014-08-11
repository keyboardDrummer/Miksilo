package transformations.javac.methods

import core.grammar.{Grammar, seqr}
import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.coreInstructions.{InvokeStaticC, InvokeVirtualC}
import transformations.javac.base.{ClassOrObjectReference, MethodAndClassC, MethodCompiler, MethodId}
import transformations.javac.expressions.ExpressionC

object CallC extends GrammarTransformation {

  override def inject(state: TransformationState): Unit = {
    ExpressionC.getGetTypeRegistry(state).put(CallKey, (call: MetaObject) => {
      val compiler = MethodAndClassC.getMethodCompiler(state)
      val methodKey = getMethodKey(call, compiler)
      val methodInfo = compiler.classCompiler.compiler.find(methodKey)
      val returnType = ByteCodeSkeleton.getMethodDescriptorReturnType(methodInfo.descriptor)
      returnType
    })
    ExpressionC.getExpressionToLines(state).put(CallKey, (call: MetaObject) => {
      val methodCompiler = MethodAndClassC.getMethodCompiler(state)
      callToLines(call, methodCompiler)
    })
  }

  def callToLines(call: MetaObject, compiler: MethodCompiler): Seq[MetaObject] = {
    val callCallee = getCallCallee(call)
    val objectExpression = SelectorC.getSelectorObject(callCallee)
    val methodKey: MethodId = getMethodKey(call, compiler)
    val methodInfo = compiler.classCompiler.compiler.find(methodKey)
    val staticCall = methodInfo._static
    val expressionToInstruction = ExpressionC.getToInstructions(compiler.transformationState)
    val calleeInstructions =
      if (!staticCall) expressionToInstruction(objectExpression)
      else Seq[MetaObject]()
    val callArguments = getCallArguments(call)
    val argumentInstructions = callArguments.flatMap(argument => expressionToInstruction(argument))
    val methodRefIndex = compiler.classCompiler.getMethodRefIndex(methodKey)
    val invokeInstructions = Seq(if (staticCall)
      InvokeStaticC.invokeStatic(methodRefIndex)
    else
      InvokeVirtualC.invokeVirtual(methodRefIndex))
    calleeInstructions ++ argumentInstructions ++ invokeInstructions
  }

  def getMethodKey(call: MetaObject, compiler: MethodCompiler) = {
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
    val expression = grammars.find(ExpressionC.ExpressionGrammar)
    val callArguments: Grammar = "(" ~> expression.manySeparated(",") <~ ")"
    val parseCall = expression ~ callArguments ^^ { case callee seqr arguments => call(callee, arguments)}
    expression.inner = expression.inner | parseCall
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
