package deltas.javac.methods.call

import core.deltas.Contract
import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.Node
import deltas.bytecode.coreInstructions.InvokeVirtualDelta
import deltas.javac.classes.MethodQuery
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.expressions.ExpressionSkeleton
import deltas.javac.methods.call.CallDelta.Call

object CallInstanceDelta extends GenericCall {

  override def description: String = "Enables calling instance methods."

  override def toByteCode(call: NodePath, compilation: Compilation): Seq[Node] = {
    val compiler = JavaClassSkeleton.getClassCompiler(compilation)

    val methodKey: MethodQuery = getMethodKey(call, compiler)
    val methodRefIndex = compiler.getMethodRefIndex(methodKey)
    getInstructionsGivenMethodRefIndex(call, compilation, methodRefIndex)
  }

  def getInstructionsGivenMethodRefIndex(path: NodePath, compilation: Compilation, methodRef: Node): Seq[Node] = {
    val call: Call[NodePath] = path
    val callCallee = call.callee
    val objectExpression = callCallee.target
    val expressionToInstruction = ExpressionSkeleton.getToInstructions(compilation)
    val calleeInstructions = expressionToInstruction(objectExpression)
    val invokeInstructions = Seq(InvokeVirtualDelta.invokeVirtual(methodRef))
    getGenericCallInstructions(call, compilation, calleeInstructions, invokeInstructions)
  }

  override def dependencies: Set[Contract] = Set(InvokeVirtualDelta) ++ super.dependencies
}
