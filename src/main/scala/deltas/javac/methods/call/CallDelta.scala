package deltas.javac.methods.call

import core.deltas.path.NodePath
import core.language.node._
import core.language.{Compilation, SourceElement}
import core.smarts.ConstraintBuilder
import core.smarts.objects.DeclarationVariable
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{FunctionType, Type}
import deltas.javac.expressions.ExpressionSkeleton
import deltas.javac.methods.MemberSelectorDelta.{Member, MemberSelector}

object CallDelta
{
  object CallKey extends NodeShape

  object CallCallee extends NodeField

  object CallArguments extends NodeField

  object CallArgumentsGrammar extends GrammarKey

  implicit class Call[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def callee: MemberSelector[T] = node(Member).asInstanceOf[T]
    def arguments: Seq[T] = NodeWrapper.wrapList(node(CallArguments).asInstanceOf[Seq[T]])
  }

  def call(callee: Any, arguments: Any): Node =
    call(callee.asInstanceOf[Node], arguments.asInstanceOf[Seq[Node]])

  def call(callee: Node, arguments: Seq[Node] = Seq()) = {
    new Node(CallDelta.CallKey, CallDelta.CallCallee -> callee, CallDelta.CallArguments -> arguments)
  }

  def callConstraints(compilation: Compilation, builder: ConstraintBuilder, call: Call[NodePath], parentScope: Scope,
                      methodName: String, nameOrigin: SourceElement, returnType: Type): DeclarationVariable = {
    val callArguments = call.arguments
    val callTypes = callArguments.map(argument => ExpressionSkeleton.getType(compilation, builder, argument, parentScope))
    val constructorType = FunctionType.curry(callTypes, returnType)
    builder.resolve(methodName, nameOrigin, parentScope, Some(constructorType))
  }
}
