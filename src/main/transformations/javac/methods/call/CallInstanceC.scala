package transformations.javac.methods.call

import core.particles.node.Node
import core.particles.path.Path
import core.particles.{CompilationState, Contract}
import transformations.bytecode.coreInstructions.InvokeVirtualC
import transformations.javac.classes.MethodId
import transformations.javac.classes.skeleton.JavaClassSkeleton
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.methods.MemberSelector

object CallInstanceC extends GenericCall {

  override def description: String = "Enables calling instance methods."

  override def toByteCode(call: Path, state: CompilationState): Seq[Node] = {
    val compiler = JavaClassSkeleton.getClassCompiler(state)

    val methodKey: MethodId = getMethodKey(call, compiler)
    val methodRefIndex = compiler.getMethodRefIndex(methodKey)
    getInstructionsGivenMethodRefIndex(call, state, methodRefIndex)
  }

  def getInstructionsGivenMethodRefIndex(call: Path, state: CompilationState, methodRefIndex: Int): Seq[Node] = {
    val callCallee = CallC.getCallCallee(call)
    val objectExpression = MemberSelector.getSelectorObject(callCallee)
    val expressionToInstruction = ExpressionSkeleton.getToInstructions(state)
    val calleeInstructions = expressionToInstruction(objectExpression)
    val invokeInstructions = Seq(InvokeVirtualC.invokeVirtual(methodRefIndex))
    getGenericCallInstructions(call, state, calleeInstructions, invokeInstructions)
  }

  override def dependencies: Set[Contract] = Set(InvokeVirtualC) ++ super.dependencies
}
