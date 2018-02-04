package deltas.javac.methods.call

import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.deltas.path.NodePath
import core.deltas.{Compilation, Contract}
import core.language.Language
import core.nabl.ConstraintBuilder
import core.nabl.scopes.objects.Scope
import core.nabl.types.objects.Type
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.classes.{ClassCompiler, ClassOrObjectReference, MethodQuery}
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import deltas.javac.methods.MemberSelector
import deltas.javac.methods.MemberSelector.MethodContainerExpressionShape
import deltas.javac.methods.call.CallDelta.CallArgumentsGrammar
import deltas.javac.types.MethodType._



trait GenericCall extends ExpressionInstance {

  override def dependencies: Set[Contract] = Set(MemberSelector)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val core = find(ExpressionSkeleton.CoreGrammar)
    val expression = find(ExpressionSkeleton.ExpressionGrammar)
    val selectorGrammar = find(MemberSelector.SelectGrammar)
    val calleeGrammar = create(CallDelta.CallCallee, selectorGrammar)
    val callArguments = create(CallArgumentsGrammar, "(" ~> expression.manySeparated(",") ~< ")")
    val parseCall = calleeGrammar.as(CallDelta.CallCallee) ~ callArguments.as(CallDelta.CallArguments) asNode CallDelta.CallKey
    core.addOption(parseCall)
  }

  override val key = CallDelta.CallKey

  override def getType(call: NodePath, compilation: Compilation): Node = {
    val compiler = JavaClassSkeleton.getClassCompiler(compilation)
    val methodKey = getMethodKey(call, compiler)
    val methodInfo = compiler.javaCompiler.find(methodKey)
    val returnType = methodInfo._type.returnType
    returnType
  }

  def getGenericCallInstructions(call: NodePath, compilation: Compilation, calleeInstructions: Seq[Node], invokeInstructions: Seq[Node]): Seq[Node] = {
    val expressionToInstruction = ExpressionSkeleton.getToInstructions(compilation)
    val callArguments = CallDelta.getCallArguments(call)
    val argumentInstructions = callArguments.flatMap(argument => expressionToInstruction(argument))
    calleeInstructions ++ argumentInstructions ++ invokeInstructions
  }

  def getMethodKey(call: NodePath, compiler: ClassCompiler): MethodQuery = {
    val callCallee = CallDelta.getCallCallee(call)
    val objectExpression = MemberSelector.getSelectorTarget(callCallee)
    val kind = MemberSelector.getReferenceKind(compiler, objectExpression).asInstanceOf[ClassOrObjectReference]

    val callArguments = CallDelta.getCallArguments(call)
    val callTypes: Seq[Node] = callArguments.map(argument => ExpressionSkeleton.getType(compiler.compilation)(argument))

    val member = MemberSelector.getSelectorMember(callCallee)
    MethodQuery(kind.info.getQualifiedName, member, callTypes)
  }

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, call: NodePath, _type: Type, parentScope: Scope): Unit = {
    val callCallee = CallDelta.getCallCallee(call)
    val selectorTarget = MemberSelector.getSelectorTarget(callCallee)
    val methodContainerExpressionShape = selectorTarget.shape.asInstanceOf[MethodContainerExpressionShape]
    val methodContainerScope = methodContainerExpressionShape.getScope(compilation, builder, selectorTarget, parentScope)
    val member = MemberSelector.getSelectorMember(callCallee)
    CallDelta.callConstraints(compilation,builder, call, methodContainerScope, member, _type)
  }
}
