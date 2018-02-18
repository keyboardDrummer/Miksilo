package deltas.javac.methods.call

import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.deltas.path.NodePath
import core.deltas.Contract
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.objects.Reference
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.classes.{ClassCompiler, ClassOrObjectReference, MethodQuery}
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import deltas.javac.methods.MemberSelectorDelta
import deltas.javac.methods.call.CallDelta.{Call, CallArgumentsGrammar}
import deltas.javac.types.MethodType._

trait GenericCall extends ExpressionInstance {

  override def dependencies: Set[Contract] = Set(MemberSelectorDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val core = find(ExpressionSkeleton.CoreGrammar)
    val expression = find(ExpressionSkeleton.ExpressionGrammar)
    val selectorGrammar = find(MemberSelectorDelta.SelectGrammar)
    val calleeGrammar = create(CallDelta.CallCallee, selectorGrammar)
    val callArguments = create(CallArgumentsGrammar, "(" ~> expression.manySeparated(",") ~< ")")
    val parseCall = calleeGrammar.as(CallDelta.CallCallee) ~ callArguments.as(CallDelta.CallArguments) asNode CallDelta.CallKey
    core.addOption(parseCall)
  }

  override val shape = CallDelta.CallKey

  override def getType(path: NodePath, compilation: Compilation): Node = {
    val call: Call[NodePath] = path
    val compiler = JavaClassSkeleton.getClassCompiler(compilation)
    val methodKey = getMethodKey(call, compiler)
    val methodInfo = compiler.javaCompiler.find(methodKey)
    val returnType = methodInfo._type.returnType
    returnType
  }

  def getGenericCallInstructions(call: Call[NodePath], compilation: Compilation, calleeInstructions: Seq[Node], invokeInstructions: Seq[Node]): Seq[Node] = {
    val expressionToInstruction = ExpressionSkeleton.getToInstructions(compilation)
    val callArguments = call.arguments
    val argumentInstructions = callArguments.flatMap(argument => expressionToInstruction(argument))
    calleeInstructions ++ argumentInstructions ++ invokeInstructions
  }

  def getMethodKey(call: Call[NodePath], compiler: ClassCompiler): MethodQuery = {
    val callCallee = call.callee
    val objectExpression = callCallee.target
    val kind = MemberSelectorDelta.getReferenceKind(compiler, objectExpression).asInstanceOf[ClassOrObjectReference]

    val callArguments = call.arguments
    val callTypes: Seq[Node] = callArguments.map(argument => ExpressionSkeleton.getType(compiler.compilation)(argument))

    val member = callCallee.member
    MethodQuery(kind.info.getQualifiedName, member, callTypes)
  }

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, returnType: Type, parentScope: Scope): Unit = {
    val call: Call[NodePath] = path
    val callCallee = call.callee
    val calleeTarget = callCallee.target
    val calleeMember = callCallee.member
    val calleeDeclaration = MemberSelectorDelta.getResolvedToDeclaration(compilation, builder, callCallee, parentScope)
    val calleeReference = new Reference(calleeMember, Some(callCallee.getLocation(MemberSelectorDelta.Member)))
    CallDelta.callConstraints(compilation, builder, call, parentScope, calleeReference, calleeDeclaration, returnType)
  }
}
