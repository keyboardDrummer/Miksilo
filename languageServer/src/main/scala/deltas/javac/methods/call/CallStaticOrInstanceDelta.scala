package deltas.javac.methods.call

import core.deltas._
import core.language.node.Node
import core.deltas.path.NodePath
import core.language.Compilation
import deltas.javac.classes._
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.expressions.ConvertsToByteCode

object CallStaticOrInstanceDelta extends GenericCall with ConvertsToByteCode {

  override def description: String = "Enables calling static and virtual methods."

  override def dependencies: Set[Contract] = CallStaticDelta.dependencies ++ CallInstanceDelta.dependencies

  override def toByteCode(call: NodePath, compilation: Compilation): Seq[Node] = {
    val compiler = JavaClassSkeleton.getClassCompiler(compilation)

    val methodKey: MethodQuery = getMethodKey(call, compiler)
    val methodRef = compiler.getMethodRefIndex(methodKey)

    val methodInfo = compiler.javaCompiler.find(methodKey)
    val staticCall = methodInfo._static
    if (staticCall)
    {
      CallStaticDelta.getInstructionsGivenMethodRefIndex(call, compilation, methodRef)
    } else
    {
      CallInstanceDelta.getInstructionsGivenMethodRefIndex(call, compilation, methodRef)
    }
  }
}


