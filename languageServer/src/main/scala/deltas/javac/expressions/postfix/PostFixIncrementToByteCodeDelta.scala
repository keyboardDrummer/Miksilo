package deltas.javac.expressions.postfix

import core.deltas.Contract
import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.{Node, NodeShape}
import deltas.bytecode.coreInstructions.integers.{IncrementIntegerDelta, LoadIntegerDelta}
import deltas.expression.PostFixIncrementDelta
import deltas.javac.expressions.ConvertsToByteCodeDelta
import deltas.javac.methods.MethodDelta
import deltas.javac.methods.assignment.SimpleAssignmentDelta

object PostFixIncrementToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def dependencies: Set[Contract] = Set(IncrementIntegerDelta, PostFixIncrementDelta, MethodDelta)

  override def toByteCode(plusPlus: NodePath, compilation: Compilation): Seq[Node] = {
    val methodCompiler = MethodDelta.getMethodCompiler(compilation)
    val name: String = plusPlus.getValue(SimpleAssignmentDelta.Target).asInstanceOf[String]
    val variableAddress = methodCompiler.getVariables(plusPlus)(name).offset
    Seq(LoadIntegerDelta.load(variableAddress), IncrementIntegerDelta.integerIncrement(variableAddress, 1))
  }

  override def description: String = "Converts post fix increment to bytecode"

  override def shape: NodeShape = PostFixIncrementDelta.Shape
}
