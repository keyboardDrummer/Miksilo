package miksilo.modularLanguages.deltas.javac.methods.call

import miksilo.modularLanguages.core.deltas.Contract
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.InvokeVirtualDelta
import miksilo.modularLanguages.deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}
import miksilo.modularLanguages.deltas.javac.methods.MemberSelectorDelta.MemberSelector
import miksilo.modularLanguages.deltas.javac.methods.call.CallDelta.Call

object CallInstanceDelta extends ConvertsToByteCodeDelta {

  override def description: String = "Enables calling instance methods."

  override def toByteCode(call: NodePath, compilation: Compilation): Seq[Node] = {
    val methodRefIndex = CallDelta.getMethodRefIndexFromCallee(compilation, call.callee)
    getInstructionsGivenMethodRefIndex(call, compilation, methodRefIndex)
  }

  def getInstructionsGivenMethodRefIndex(path: NodePath, compilation: Compilation, methodRef: Node): Seq[Node] = {
    val call: Call[NodePath] = path
    val callCallee: MemberSelector[NodePath] = call.callee
    val objectExpression = callCallee.target
    val expressionToInstruction = ToByteCodeSkeleton.getToInstructions(compilation)
    val calleeInstructions = expressionToInstruction(objectExpression)
    val invokeInstructions = Seq(InvokeVirtualDelta.invokeVirtual(methodRef))
    CallToByteCode.getGenericCallInstructions(call, compilation, calleeInstructions, invokeInstructions)
  }

  override def dependencies: Set[Contract] = Set(InvokeVirtualDelta, CallMemberDelta)

  override def shape = CallDelta.Shape
}
