package deltas.javac.methods.call

import core.deltas.Contract
import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.Node
import deltas.bytecode.coreInstructions.InvokeStaticDelta
import deltas.javac.expressions.ConvertsToByteCodeDelta
import deltas.javac.methods.call.CallDelta.Call

object CallStaticDelta extends ConvertsToByteCodeDelta {

  override def description: String = "Enables calling static methods."

  override def toByteCode(path: NodePath, compilation: Compilation): Seq[Node] = {
    val call: Call[NodePath] = path
    val methodRefIndex = CallDelta.getMethodRefIndexFromCallee(compilation, call.callee)
    getInstructionsGivenMethodRefIndex(call, compilation, methodRefIndex)
  }

  def getInstructionsGivenMethodRefIndex(call: NodePath, compilation: Compilation, methodRef: Node): Seq[Node] = {
    val calleeInstructions = Seq[Node]()
    val invokeInstructions = Seq(InvokeStaticDelta.invokeStatic(methodRef))
    CallToByteCode.getGenericCallInstructions(call, compilation, calleeInstructions, invokeInstructions)
  }

  override def dependencies: Set[Contract] = Set(InvokeStaticDelta, CallMemberDelta)

  override def shape = CallDelta.Shape
}
