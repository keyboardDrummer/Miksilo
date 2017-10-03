package transformations.javac.methods.call

import core.particles.node.Node
import core.particles.path.Path
import core.particles.{Compilation, Contract, Language}
import transformations.bytecode.coreInstructions.InvokeVirtualDelta
import transformations.javac.classes.MethodQuery
import transformations.javac.classes.skeleton.JavaClassSkeleton
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.methods.MemberSelector

object CallInstanceC extends GenericCall {

  override def description: String = "Enables calling instance methods."

  override def toByteCode(call: Path, state: Language): Seq[Node] = {
    val compiler = JavaClassSkeleton.getClassCompiler(state)

    val methodKey: MethodQuery = getMethodKey(call, compiler)
    val methodRefIndex = compiler.getMethodRefIndex(methodKey)
    getInstructionsGivenMethodRefIndex(call, state, methodRefIndex)
  }

  def getInstructionsGivenMethodRefIndex(call: Path, state: Language, methodRef: Node): Seq[Node] = {
    val callCallee = CallC.getCallCallee(call)
    val objectExpression = MemberSelector.getSelectorObject(callCallee)
    val expressionToInstruction = ExpressionSkeleton.getToInstructions(state)
    val calleeInstructions = expressionToInstruction(objectExpression)
    val invokeInstructions = Seq(InvokeVirtualDelta.invokeVirtual(methodRef))
    getGenericCallInstructions(call, state, calleeInstructions, invokeInstructions)
  }

  override def dependencies: Set[Contract] = Set(InvokeVirtualDelta) ++ super.dependencies
}
