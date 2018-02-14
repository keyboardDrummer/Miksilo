package deltas.javac.methods.call

import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node._
import core.smarts.ConstraintBuilder
import core.smarts.objects.Declaration
import core.smarts.scopes.objects.Scope
import core.smarts.types.DeclarationOfType
import core.smarts.types.objects.{FunctionType, Type}
import deltas.javac.expressions.ExpressionSkeleton
import deltas.javac.methods.MemberSelectorDelta.MemberSelector

object CallDelta
{
  object CallKey extends NodeShape

  object CallCallee extends NodeField

  object CallArguments extends NodeField

  object CallArgumentsGrammar extends GrammarKey

  implicit class Call[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def callee: MemberSelector[T] = node(CallCallee).asInstanceOf[T]
    def arguments: Seq[T] = NodeWrapper.wrapList(node(CallArguments).asInstanceOf[Seq[T]])
  }

  def call(callee: Any, arguments: Any): Node =
    call(callee.asInstanceOf[Node], arguments.asInstanceOf[Seq[Node]])

  def call(callee: Node, arguments: Seq[Node] = Seq()) = {
    new Node(CallDelta.CallKey, CallDelta.CallCallee -> callee, CallDelta.CallArguments -> arguments)
  }

  def callConstraints(compilation: Compilation, builder: ConstraintBuilder, call: Call[NodePath], parentScope: Scope,
                      methodDeclaration: Declaration, returnType: Type): Unit = {
    val callArguments = call.arguments
    val callTypes = callArguments.map(argument => ExpressionSkeleton.getType(compilation, builder, argument, parentScope))
    val functionType = FunctionType.curry(callTypes, returnType)
    builder.add(DeclarationOfType(methodDeclaration, functionType))
  }
}
