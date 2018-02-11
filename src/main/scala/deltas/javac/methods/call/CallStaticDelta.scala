package deltas.javac.methods.call

import core.language.node.Node
import core.deltas.path.NodePath
import core.deltas.Contract
import core.language.Compilation
import deltas.bytecode.coreInstructions.InvokeStaticDelta
import deltas.javac.classes.MethodQuery
import deltas.javac.classes.skeleton.JavaClassSkeleton

object CallStaticDelta extends GenericCall {

  override def description: String = "Enables calling static methods."

  override def toByteCode(call: NodePath, compilation: Compilation): Seq[Node] = {
    val compiler = JavaClassSkeleton.getClassCompiler(compilation)

    val methodKey: MethodQuery = getMethodKey(call, compiler)
    val methodRefIndex = compiler.getMethodRefIndex(methodKey)
    getInstructionsGivenMethodRefIndex(call, compilation, methodRefIndex)
  }

  def getInstructionsGivenMethodRefIndex(call: NodePath, compilation: Compilation, methodRef: Node): Seq[Node] = {
    val calleeInstructions = Seq[Node]()
    val invokeInstructions = Seq(InvokeStaticDelta.invokeStatic(methodRef))
    getGenericCallInstructions(call, compilation, calleeInstructions, invokeInstructions)
  }

  override def dependencies: Set[Contract] = Set(InvokeStaticDelta) ++ super.dependencies
}
