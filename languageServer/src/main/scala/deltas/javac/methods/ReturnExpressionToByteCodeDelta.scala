package deltas.javac.methods

import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.Node
import deltas.bytecode.coreInstructions.floats.FloatReturnInstructionDelta
import deltas.bytecode.coreInstructions.integers.IntegerReturnInstructionDelta
import deltas.bytecode.coreInstructions.longs.LongReturnInstructionDelta
import deltas.bytecode.coreInstructions.objects.AddressReturnInstructionDelta
import deltas.bytecode.types._
import deltas.expression.ExpressionDelta
import deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}
import deltas.javac.methods.ReturnExpressionDelta.getReturnValue

object ReturnExpressionToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def toByteCode(_return: NodePath, compilation: Compilation): Seq[Node] = {
    val methodCompiler = MethodDelta.getMethodCompiler(compilation)
    returnToLines(_return, methodCompiler)
  }

  def returnToLines(_return: NodePath, compiler: MethodCompiler): Seq[Node] = {
    val returnValue: NodePath = getReturnValue(_return)
    val returnValueInstructions = ToByteCodeSkeleton.getToInstructions(compiler.compilation)(returnValue)
    val getType = ExpressionDelta.getType(compiler.compilation)
    returnValueInstructions ++ (getType(returnValue) match
    {
      case x if x == IntTypeDelta.intType => Seq(IntegerReturnInstructionDelta.integerReturn)
      case x if x == LongTypeDelta.longType => Seq(LongReturnInstructionDelta.longReturn)
      case x if x == FloatTypeDelta.floatType => Seq(FloatReturnInstructionDelta.create)
      case x if x == DoubleTypeDelta.doubleType => Seq(LongReturnInstructionDelta.longReturn)
      case x if TypeSkeleton.getSuperTypes(compiler.compilation)(x).
        contains(QualifiedObjectTypeDelta.rootObjectType) => Seq(AddressReturnInstructionDelta.create)
      case _ => throw new NotImplementedError()
    })
  }

  override def description = "Convert the return expression to bytecode"

  override def dependencies = Set(ReturnExpressionDelta, MethodDelta, IntegerReturnInstructionDelta)

  override def shape = ReturnExpressionDelta.Shape
}
