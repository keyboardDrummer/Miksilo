package miksilo.modularLanguages.deltas.javac.expressions.postfix

import miksilo.modularLanguages.core.deltas.Contract
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.{Node, NodeShape}
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.integers.{IncrementIntegerDelta, LoadIntegerDelta}
import miksilo.modularLanguages.deltas.expression.{PostFixIncrementDelta, VariableDelta}
import miksilo.modularLanguages.deltas.javac.expressions.ConvertsToByteCodeDelta
import miksilo.modularLanguages.deltas.javac.methods.MethodDelta
import miksilo.modularLanguages.deltas.statement.assignment.SimpleAssignmentDelta

object PostFixIncrementToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def dependencies: Set[Contract] = Set(IncrementIntegerDelta, PostFixIncrementDelta, MethodDelta)

  override def toByteCode(plusPlus: NodePath, compilation: Compilation): Seq[Node] = {
    val methodCompiler = MethodDelta.getMethodCompiler(compilation)
    val target = plusPlus(SimpleAssignmentDelta.Target).asInstanceOf[NodePath]
    target.current.shape match {
      case VariableDelta.Shape =>
        val name = VariableDelta.Variable[Node](target.current).name
        val variableAddress = methodCompiler.getVariables(plusPlus)(name).offset
        Seq(LoadIntegerDelta.load(variableAddress), IncrementIntegerDelta.integerIncrement(variableAddress, 1))
    }
  }

  override def description: String = "Converts post fix increment to bytecode"

  override def shape: NodeShape = PostFixIncrementDelta.Shape
}
