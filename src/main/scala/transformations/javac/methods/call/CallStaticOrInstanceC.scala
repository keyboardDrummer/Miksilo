package transformations.javac.methods.call

import core.particles._
import core.particles.node.Node
import core.particles.path.Path
import transformations.javac.classes._
import transformations.javac.classes.skeleton.JavaClassSkeleton

object CallStaticOrInstanceC extends GenericCall {

  override def description: String = "Enables calling static and virtual methods."

  override def dependencies: Set[Contract] = CallStaticC.dependencies ++ CallInstanceC.dependencies

  override def toByteCode(call: Path, state: CompilationState): Seq[Node] = {
    val compiler = JavaClassSkeleton.getClassCompiler(state)

    val methodKey: MethodQuery = getMethodKey(call, compiler)
    val methodRef = compiler.getMethodRefIndex(methodKey)

    val methodInfo = compiler.compiler.find(methodKey)
    val staticCall = methodInfo._static
    if (staticCall)
    {
      CallStaticC.getInstructionsGivenMethodRefIndex(call, state, methodRef)
    } else
    {
      CallInstanceC.getInstructionsGivenMethodRefIndex(call, state, methodRef)
    }
  }
}


