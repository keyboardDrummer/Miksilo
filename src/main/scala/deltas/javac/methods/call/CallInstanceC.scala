package deltas.javac.methods.call

import core.deltas.node.Node
import core.deltas.path.Path
import core.deltas.{Compilation, Contract}
import deltas.bytecode.coreInstructions.InvokeVirtualDelta
import deltas.javac.classes.MethodQuery
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.expressions.ExpressionSkeleton
import deltas.javac.methods.MemberSelector

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
