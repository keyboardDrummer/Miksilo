package transformations.javac.methods.call

import core.particles._
import core.particles.node.Node
import core.particles.path.Path
import transformations.javac.classes._

object CallStaticOrInstanceC extends GenericCall {

  override def dependencies: Set[Contract] = CallStaticC.dependencies ++ CallInstanceC.dependencies

  override def toByteCode(call: Path, state: CompilationState): Seq[Node] = {
    val compiler = JavaClassSkeleton.getClassCompiler(state)

    val methodKey: MethodId = getMethodKey(call, compiler)
    val methodRefIndex = compiler.getMethodRefIndex(methodKey)

    val methodInfo = compiler.compiler.find(methodKey)
    val staticCall = methodInfo._static
    if (staticCall)
    {
      CallStaticC.getInstructionsGivenMethodRefIndex(call, state, methodRefIndex)
    } else
    {
      CallInstanceC.getInstructionsGivenMethodRefIndex(call, state, methodRefIndex)
    }
  }
}


