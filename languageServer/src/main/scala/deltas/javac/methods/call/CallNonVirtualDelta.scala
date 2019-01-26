package deltas.javac.methods.call

import core.deltas.Contract
import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.{Node, NodeShape}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.coreInstructions.InvokeSpecialDelta
import deltas.expression.ExpressionInstance
import deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}
import deltas.javac.methods.MemberSelectorDelta.MemberSelector
import deltas.javac.methods.call.CallDelta.Call

object CallNonVirtualDelta extends CallWithMemberSelector with ExpressionInstance with ConvertsToByteCodeDelta {

  override def description: String = "Call a method with no dynamic lookup"

  override def dependencies: Set[Contract] = CallStaticDelta.dependencies ++ CallInstanceDelta.dependencies

  override def toByteCode(path: NodePath, compilation: Compilation): Seq[Node] = {
    val call: Call[NodePath] = path

    val methodRefIndex = CallDelta.getMethodRefIndexFromCallee(compilation, call.callee)

    val callCallee: MemberSelector[NodePath] = call.callee
    val expressionToInstruction = ToByteCodeSkeleton.getToInstructions(compilation)
    val invokeInstructions = Seq(InvokeSpecialDelta.invokeSpecial(methodRefIndex))
    getGenericCallInstructions(call, compilation, expressionToInstruction(callCallee.target), invokeInstructions)
  }

  object Shape extends NodeShape
  override def shape = Shape

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    CallDelta.constraints(compilation, builder, expression, _type, parentScope)
  }
}
