package miksilo.modularLanguages.deltas.javac.methods.call

import miksilo.modularLanguages.core.deltas._
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.javac.expressions.ConvertsToByteCodeDelta
import miksilo.modularLanguages.deltas.method.call.CallDelta
import miksilo.modularLanguages.deltas.method.call.CallDelta.Call

object CallStaticOrInstanceDelta extends ConvertsToByteCodeDelta {

  override def description: String = "Enables calling static and virtual methods."

  override def dependencies: Set[Contract] = CallStaticDelta.dependencies ++ CallInstanceDelta.dependencies

  override def toByteCode(path: NodePath, compilation: Compilation): Seq[Node] = {
    val call: Call[NodePath] = path
    val method = CallDelta.getMethodFromCallee(compilation, call.callee)

    val methodRefIndex = CallDelta.getMethodRefIndexFromMethod(method)
    val staticCall = method.isStatic
    if (staticCall)
    {
      CallStaticDelta.getInstructionsGivenMethodRefIndex(call, compilation, methodRefIndex)
    } else
    {
      CallInstanceDelta.getInstructionsGivenMethodRefIndex(call, compilation, methodRefIndex)
    }
  }

  override def shape = CallDelta.Shape
}


