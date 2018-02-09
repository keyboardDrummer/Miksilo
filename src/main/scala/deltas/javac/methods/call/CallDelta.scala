package deltas.javac.methods.call

import core.deltas.Compilation
import core.deltas.node._
import core.deltas.path.NodePath
import core.smarts.ConstraintBuilder
import core.smarts.objects.DeclarationVariable
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{FunctionType, Type}
import deltas.javac.expressions.ExpressionSkeleton

object CallDelta
{
  object CallKey extends NodeShape

  object CallCallee extends NodeField

  object CallArguments extends NodeField

  object CallArgumentsGrammar extends GrammarKey

  def getCallCallee[T <: NodeLike](call: T) = call(CallDelta.CallCallee).asInstanceOf[T]

  def getCallArguments[T <: NodeLike](call: T) = call(CallDelta.CallArguments).asInstanceOf[Seq[T]]

  def call(callee: Any, arguments: Any): Node =
    call(callee.asInstanceOf[Node], arguments.asInstanceOf[Seq[Node]])

  def call(callee: Node, arguments: Seq[Node] = Seq()) = {
    new Node(CallDelta.CallKey, CallDelta.CallCallee -> callee, CallDelta.CallArguments -> arguments)
  }

  def callConstraints(compilation: Compilation, builder: ConstraintBuilder, call: NodePath, parentScope: Scope,
                      methodName: String, returnType: Type): DeclarationVariable = {
    val callArguments = CallDelta.getCallArguments(call)
    val callTypes = callArguments.map(argument => ExpressionSkeleton.getType(compilation, builder, argument, parentScope))
    val constructorType = FunctionType.curry(callTypes, returnType)
    builder.resolve(methodName, call, parentScope, Some(constructorType))
  }
}
