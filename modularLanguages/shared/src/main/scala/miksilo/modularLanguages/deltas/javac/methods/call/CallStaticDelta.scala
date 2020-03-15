package miksilo.modularLanguages.deltas.javac.methods.call

import miksilo.modularLanguages.core.deltas.Contract
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.InvokeStaticDelta
import miksilo.modularLanguages.deltas.javac.expressions.ConvertsToByteCodeDelta
import miksilo.modularLanguages.deltas.method.call.CallDelta
import miksilo.modularLanguages.deltas.method.call.CallDelta.Call

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
