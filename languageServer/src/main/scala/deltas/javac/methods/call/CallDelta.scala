package deltas.javac.methods.call

import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node._
import core.smarts.objects.Reference
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{FunctionType, Type}
import core.smarts.{ConstraintBuilder, ResolvesToType}
import deltas.javac.expressions.ByteCodeExpressionSkeleton
import deltas.javac.methods.MemberSelectorDelta.MemberSelector

object CallDelta
{
  object Shape extends NodeShape

  object Callee extends NodeField

  object Arguments extends NodeField

  object CallArgumentsGrammar extends GrammarKey

  implicit class Call[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def callee: MemberSelector[T] = node(Callee).asInstanceOf[T]
    def arguments: Seq[T] = NodeWrapper.wrapList(node(Arguments).asInstanceOf[Seq[T]])
  }

  def call(callee: Any, arguments: Any): Node =
    call(callee.asInstanceOf[Node], arguments.asInstanceOf[Seq[Node]])

  def call(callee: Node, arguments: Seq[Node] = Seq()): Node = {
    new Node(CallDelta.Shape, CallDelta.Callee -> callee, CallDelta.Arguments -> arguments)
  }

  def callConstraints(compilation: Compilation, builder: ConstraintBuilder, callArguments: Seq[NodePath], parentScope: Scope,
                      methodReference: Reference, returnType: Type): Unit = {
    val callTypes = callArguments.map(argument => ByteCodeExpressionSkeleton.getType(compilation, builder, argument, parentScope))
    val functionType = FunctionType.curry(callTypes, returnType)
    builder.add(new ResolvesToType(methodReference, builder.declarationVariable(), functionType))
  }
}
