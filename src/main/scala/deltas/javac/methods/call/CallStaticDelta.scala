package deltas.javac.methods.call

import core.deltas.node.Node
import core.deltas.path.Path
import core.deltas.{Compilation, Contract}
import deltas.bytecode.coreInstructions.InvokeStaticDelta
import deltas.javac.classes.MethodQuery
import deltas.javac.classes.skeleton.JavaClassSkeleton

object CallStaticDelta extends GenericCall {

  override def description: String = "Enables calling static methods."

  override def toByteCode(call: Path, compilation: Compilation): Seq[Node] = {
    val compiler = JavaClassSkeleton.getClassCompiler(compilation)

    val methodKey: MethodQuery = getMethodKey(call, compiler)
    val methodRefIndex = compiler.getMethodRefIndex(methodKey)
    getInstructionsGivenMethodRefIndex(call, compilation, methodRefIndex)
  }

  def getInstructionsGivenMethodRefIndex(call: Path, compilation: Compilation, methodRef: Node): Seq[Node] = {
    val calleeInstructions = Seq[Node]()
    val invokeInstructions = Seq(InvokeStaticDelta.invokeStatic(methodRef))
    getGenericCallInstructions(call, compilation, calleeInstructions, invokeInstructions)
  }

  override def dependencies: Set[Contract] = Set(InvokeStaticDelta) ++ super.dependencies
}
