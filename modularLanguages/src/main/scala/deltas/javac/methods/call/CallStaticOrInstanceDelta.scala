package deltas.javac.methods.call

import core.deltas._
import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.Node
import deltas.javac.expressions.ConvertsToByteCodeDelta
import deltas.javac.methods.call.CallDelta.Call

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


