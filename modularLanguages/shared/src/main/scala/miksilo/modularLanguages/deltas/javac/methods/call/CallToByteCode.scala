package miksilo.modularLanguages.deltas.javac.methods.call

import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.javac.expressions.ToByteCodeSkeleton
import miksilo.modularLanguages.deltas.javac.methods.call.CallDelta.Call

object CallToByteCode {

  def getGenericCallInstructions(call: Call[NodePath], compilation: Compilation, calleeInstructions: Seq[Node], invokeInstructions: Seq[Node]): Seq[Node] = {
    val expressionToInstruction = ToByteCodeSkeleton.getToInstructions(compilation)
    val callArguments = call.arguments
    val argumentInstructions = callArguments.flatMap(argument => expressionToInstruction(argument))
    calleeInstructions ++ argumentInstructions ++ invokeInstructions
  }
}
