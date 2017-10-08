package transformations.javac.methods.call

import core.particles.node.Node
import core.particles.path.Path
import core.particles.{Compilation, Contract}
import transformations.bytecode.coreInstructions.InvokeVirtualDelta
import transformations.javac.classes.MethodQuery
import transformations.javac.classes.skeleton.JavaClassSkeleton
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.methods.MemberSelector

object CallInstanceC extends GenericCall {

  override def description: String = "Enables calling instance methods."

  override def toByteCode(call: Path, compilation: Compilation): Seq[Node] = {
    val compiler = JavaClassSkeleton.getClassCompiler(compilation)

    val methodKey: MethodQuery = getMethodKey(call, compiler)
    val methodRefIndex = compiler.getMethodRefIndex(methodKey)
    getInstructionsGivenMethodRefIndex(call, compilation, methodRefIndex)
  }

  def getInstructionsGivenMethodRefIndex(call: Path, compilation: Compilation, methodRef: Node): Seq[Node] = {
    val callCallee = CallC.getCallCallee(call)
    val objectExpression = MemberSelector.getSelectorObject(callCallee)
    val expressionToInstruction = ExpressionSkeleton.getToInstructions(compilation)
    val calleeInstructions = expressionToInstruction(objectExpression)
    val invokeInstructions = Seq(InvokeVirtualDelta.invokeVirtual(methodRef))
    getGenericCallInstructions(call, compilation, calleeInstructions, invokeInstructions)
  }

  override def dependencies: Set[Contract] = Set(InvokeVirtualDelta) ++ super.dependencies
}
