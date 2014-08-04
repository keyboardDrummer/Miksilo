package transformations.javac.methods

import core.grammar.{Grammar, seqr}
import core.transformation._
import transformations.bytecode.instructions.{InvokeStaticC, InvokeVirtualC}
import transformations.javac.base.{ClassOrObjectReference, JavaMethodC, MethodCompiler, MethodId}
import transformations.javac.expressions.ExpressionC

object CallC extends GrammarTransformation {

  override def inject(state: TransformationState): Unit = {
    ExpressionC.getExpressionToLines(state).put(CallKey, (call: MetaObject) => {
      val methodCompiler = JavaMethodC.getMethodCompiler(state)
      callToLines(call, methodCompiler)
    })
  }

  def callToLines(call: MetaObject, compiler: MethodCompiler): Seq[MetaObject] = {
    val callCallee = getCallCallee(call)
    val objectExpression = SelectorC.getSelectorObject(callCallee)
    val member = SelectorC.getSelectorMember(callCallee)
    val kind = compiler.getReferenceKind(objectExpression).asInstanceOf[ClassOrObjectReference]

    val methodKey: MethodId = new MethodId(kind.info.getQualifiedName, member)
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
