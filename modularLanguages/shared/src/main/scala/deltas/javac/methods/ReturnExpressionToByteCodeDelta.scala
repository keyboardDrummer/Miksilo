package deltas.javac.methods

import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.Node
import deltas.bytecode.coreInstructions.floats.FloatReturnInstructionDelta
import deltas.bytecode.coreInstructions.integers.IntegerReturnInstructionDelta
import deltas.bytecode.coreInstructions.longs.LongReturnInstructionDelta
import deltas.bytecode.types._
import deltas.expression.ExpressionDelta
import deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}

object ReturnExpressionToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def toByteCode(_return: NodePath, compilation: Compilation): Seq[Node] = {
    val methodCompiler = MethodDelta.getMethodCompiler(compilation)
    returnToLines(_return, methodCompiler)
  }

  def returnToLines(_return: NodePath, compiler: MethodCompiler): Seq[Node] = {
    val returnValue: NodePath = ReturnExpressionDelta.getReturnValue(_return)
    val returnValueInstructions = ToByteCodeSkeleton.getToInstructions(compiler.compilation)(returnValue)
    val returnType = ExpressionDelta.cachedNodeType(compiler.compilation, returnValue)
    returnValueInstructions ++ (returnType match
    {
      case IntTypeDelta.intType => Seq(IntegerReturnInstructionDelta.integerReturn)
      case LongTypeDelta.longType => Seq(LongReturnInstructionDelta.longReturn)
      case FloatTypeDelta.floatType => Seq(FloatReturnInstructionDelta.create)
      case DoubleTypeDelta.doubleType => Seq(LongReturnInstructionDelta.longReturn)
//      case x if TypeSkeleton.getSuperTypes(compiler.compilation)(x). TODO fix
//        contains(QualifiedObjectTypeDelta.rootObjectType) => Seq(AddressReturnInstructionDelta.create)
      case _ => throw new NotImplementedError()
    })
  }

  override def description = "Convert the return expression to bytecode"

  override def dependencies = Set(ReturnExpressionDelta, MethodDelta, IntegerReturnInstructionDelta)

  override def shape = ReturnExpressionDelta.Shape
}
