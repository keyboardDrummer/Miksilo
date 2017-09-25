package transformations.javac.methods.call

import core.particles.{Compilation, Contract, Language}
import core.particles.node.Node
import core.particles.path.Path
import transformations.bytecode.coreInstructions.InvokeStaticDelta
import transformations.javac.classes.MethodQuery
import transformations.javac.classes.skeleton.JavaClassSkeleton

object CallStaticC extends GenericCall {

  override def description: String = "Enables calling static methods."

  override def toByteCode(call: Path, state: Language): Seq[Node] = {
    val compiler = JavaClassSkeleton.getClassCompiler(state)

    val methodKey: MethodQuery = getMethodKey(call, compiler)
    val methodRefIndex = compiler.getMethodRefIndex(methodKey)
    getInstructionsGivenMethodRefIndex(call, state, methodRefIndex)
  }

  def getInstructionsGivenMethodRefIndex(call: Path, state: Language, methodRef: Node): Seq[Node] = {
    val calleeInstructions = Seq[Node]()
    val invokeInstructions = Seq(InvokeStaticDelta.invokeStatic(methodRef))
    getGenericCallInstructions(call, state, calleeInstructions, invokeInstructions)
  }

  override def dependencies: Set[Contract] = Set(InvokeStaticDelta) ++ super.dependencies
}
