package deltas.javac.expressions.additive

import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.Node
import deltas.bytecode.coreInstructions.integers.AddIntegersDelta
import deltas.bytecode.coreInstructions.longs.AddLongsDelta
import deltas.bytecode.types.{IntTypeDelta, LongTypeDelta}
import deltas.expression.additive.AdditionDelta
import deltas.expression.{ExpressionDelta, LeftAssociativeBinaryOperatorDelta}
import deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}

object AdditionToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def toByteCode(path: NodePath, compilation: Compilation): Seq[Node] = {
    val addition: LeftAssociativeBinaryOperatorDelta.BinaryOperator[NodePath] = path
    val toInstructions = ToByteCodeSkeleton.getToInstructions(compilation)
    val firstInstructions = toInstructions(addition.left)
    val secondInstructions = toInstructions(addition.right)
    firstInstructions ++ secondInstructions ++ (ExpressionDelta.getCachedType(compilation, path) match {
      case IntTypeDelta.constraintType => Seq(AddIntegersDelta.addIntegers())
      case LongTypeDelta.constraintType => Seq(AddLongsDelta.addLongs())
      case _ => throw new NotImplementedError()
    })
  }

  override def shape = AdditionDelta.Shape

  override def description = "Converts + to bytecode"

  override def dependencies = Set(AddIntegersDelta)
}
