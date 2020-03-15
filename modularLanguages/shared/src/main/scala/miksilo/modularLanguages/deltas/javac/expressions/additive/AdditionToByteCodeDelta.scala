package miksilo.modularLanguages.deltas.javac.expressions.additive

import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.integers.AddIntegersDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.longs.AddLongsDelta
import miksilo.modularLanguages.deltas.bytecode.types.{IntTypeDelta, LongTypeDelta}
import miksilo.modularLanguages.deltas.expression.additive.AdditionDelta
import miksilo.modularLanguages.deltas.expression.{ExpressionDelta, LeftAssociativeBinaryOperatorDelta}
import miksilo.modularLanguages.deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}

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
