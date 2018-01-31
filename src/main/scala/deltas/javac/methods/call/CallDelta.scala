package deltas.javac.methods.call

import core.deltas.node._

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
}
