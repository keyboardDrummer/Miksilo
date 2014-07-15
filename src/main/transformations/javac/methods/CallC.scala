package transformations.javac.methods

import core.grammar.{Grammar, seqr}
import core.transformation._
import transformations.bytecode.ByteCode
import transformations.javac.base.{ClassOrObjectReference, JavaMethodC, MethodCompiler, MethodId}
import transformations.javac.expressions.ExpressionC

object CallC extends GrammarTransformation {

  object CallKey

  object CallCallee

  object CallArguments

  def call(callee: MetaObject, arguments: Seq[MetaObject] = Seq()) = {
    new MetaObject(CallKey) {
      data.put(CallCallee, callee)
      data.put(CallArguments, arguments)
    }
  }

  def getCallCallee(call: MetaObject) = call(CallCallee).asInstanceOf[MetaObject]

  def getCallArguments(call: MetaObject) = call(CallArguments).asInstanceOf[Seq[MetaObject]]

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    ExpressionC.getExpressionToLines(state).putIfEmpty(CallKey, (call: MetaObject) => {
      val methodCompiler = JavaMethodC.getMethodCompiler(state)
      callToLines(call, methodCompiler)
    })
  }

  override def dependencies: Set[Contract] = Set(SelectorC)

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
      ByteCode.invokeStatic(methodRefIndex)
    else
      ByteCode.invokeVirtual(methodRefIndex))
    calleeInstructions ++ argumentInstructions ++ invokeInstructions
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val expression = grammars.find(ExpressionC.ExpressionGrammar)
    val callArguments: Grammar = "(" ~> expression.manySeparated(",") <~ ")"
    val parseCall = expression ~ callArguments ^^ { case callee seqr arguments => call(callee, arguments)}
    expression.inner = expression.inner | parseCall
  }

  def call(callee: Any, arguments: Any): MetaObject = call(callee.asInstanceOf[MetaObject], arguments.asInstanceOf[Seq[MetaObject]])

}
