package deltas.javac.methods.call

import core.deltas.path.NodePath
import core.deltas.{Contract, Delta, HasShape, ShapeProperty}
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.objects.Reference
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.expression.{ExpressionDelta, ExpressionInstance}
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.classes.{ClassCompiler, ClassOrObjectReference, MethodQuery}
import deltas.javac.expressions.ToByteCodeSkeleton
import deltas.javac.methods.MemberSelectorDelta
import deltas.javac.methods.MemberSelectorDelta.MemberSelector
import deltas.javac.methods.call.CallDelta.Call
import deltas.javac.types.MethodType._

object ReferenceExpressionSkeleton {
  val instances = new ShapeProperty[ReferenceExpression]
  def getReference(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, parentScope: Scope): Reference = {
    instances(compilation, expression.shape).getReference(compilation, builder, expression, parentScope)
  }
}

trait ReferenceExpression {
  def getReference(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, parentScope: Scope): Reference
}

trait ReferenceExpressionDelta extends Delta with HasShape with ReferenceExpression {
  override def inject(language: Language): Unit = {
    super.inject(language)
    ReferenceExpressionSkeleton.instances.add(language, shape, this)
  }
}

//TODO extend from Delta, can be done once old getType is out of ExpressionInstance.
trait CallWithMemberSelector extends Delta with ExpressionInstance {

  override def dependencies: Set[Contract] = Set(CallDelta, MemberSelectorDelta)

  override val shape = CallDelta.Shape

  override def getType(path: NodePath, compilation: Compilation): Node = {
    val call: Call[NodePath] = path
    val compiler = JavaClassSkeleton.getClassCompiler(compilation)
    val methodKey = getMethodKey(call, compiler)
    val methodInfo = compiler.javaCompiler.find(methodKey)
    val returnType = methodInfo._type.returnType
    returnType
  }

  def getGenericCallInstructions(call: Call[NodePath], compilation: Compilation, calleeInstructions: Seq[Node], invokeInstructions: Seq[Node]): Seq[Node] = {
    val expressionToInstruction = ToByteCodeSkeleton.getToInstructions(compilation)
    val callArguments = call.arguments
    val argumentInstructions = callArguments.flatMap(argument => expressionToInstruction(argument))
    calleeInstructions ++ argumentInstructions ++ invokeInstructions
  }

  def getMethodKey(call: Call[NodePath], compiler: ClassCompiler): MethodQuery = {
    val callCallee: MemberSelector[NodePath] = call.callee
    val objectExpression = callCallee.target
    val kind = MemberSelectorDelta.getReferenceKind(compiler, objectExpression).asInstanceOf[ClassOrObjectReference]

    val callArguments = call.arguments
    val callTypes: Seq[Node] = callArguments.map(argument => ExpressionDelta.getType(compiler.compilation)(argument))

    val member = callCallee.member
    MethodQuery(kind.info.getQualifiedName, member, callTypes)
  }

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, returnType: Type, parentScope: Scope): Unit = {
    val call: Call[NodePath] = path
    val calleeReference = ReferenceExpressionSkeleton.getReference(compilation, builder, call.callee, parentScope)
    CallDelta.callConstraints(compilation, builder, call.arguments, parentScope, calleeReference, returnType)
  }
}
