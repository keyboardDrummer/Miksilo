package deltas.javac.methods.call

import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.Node
import deltas.javac.expressions.ToByteCodeSkeleton
import deltas.javac.methods.call.CallDelta.Call

object CallToByteCode {

  def getGenericCallInstructions(call: Call[NodePath], compilation: Compilation, calleeInstructions: Seq[Node], invokeInstructions: Seq[Node]): Seq[Node] = {
    val expressionToInstruction = ToByteCodeSkeleton.getToInstructions(compilation)
    val callArguments = call.arguments
    val argumentInstructions = callArguments.flatMap(argument => expressionToInstruction(argument))
    calleeInstructions ++ argumentInstructions ++ invokeInstructions
  }
}
