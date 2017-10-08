package transformations.javac.methods.call

import core.particles.node.Node
import core.particles.path.Path
import core.particles.{Compilation, Contract}
import transformations.bytecode.coreInstructions.InvokeStaticDelta
import transformations.javac.classes.MethodQuery
import transformations.javac.classes.skeleton.JavaClassSkeleton

object CallStaticC extends GenericCall {

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
