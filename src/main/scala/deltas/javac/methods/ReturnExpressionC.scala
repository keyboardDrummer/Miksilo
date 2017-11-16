package deltas.javac.methods

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.deltas.path.Path
import deltas.bytecode.coreInstructions.floats.FloatReturnInstructionDelta
import deltas.bytecode.coreInstructions.integers.IntegerReturnInstructionDelta
import deltas.bytecode.coreInstructions.longs.LongReturnInstructionDelta
import deltas.bytecode.coreInstructions.objects.AddressReturnInstructionDelta
import deltas.bytecode.types._
import deltas.javac.expressions.ExpressionSkeleton
import deltas.javac.statements.{StatementInstance, StatementSkeleton}

object ReturnExpressionC extends StatementInstance {

  override def dependencies: Set[Contract] = Set(MethodDelta, IntegerReturnInstructionDelta)

  override def getNextStatements(obj: Path, labels: Map[Any, Path]): Set[Path] = Set.empty

  def returnToLines(_return: Path, compiler: MethodCompiler): Seq[Node] = {
    val returnValue: Path = getReturnValue(_return)
    val returnValueInstructions = ExpressionSkeleton.getToInstructions(compiler.compilation)(returnValue)
    val getType = ExpressionSkeleton.getType(compiler.compilation)
    returnValueInstructions ++ (getType(returnValue) match
    {
      case x if x == IntTypeC.intType => Seq(IntegerReturnInstructionDelta.integerReturn)
      case x if x == LongTypeC.longType => Seq(LongReturnInstructionDelta.longReturn)
      case x if x == FloatTypeC.floatType => Seq(FloatReturnInstructionDelta.create)
      case x if x == DoubleTypeC.doubleType => Seq(LongReturnInstructionDelta.longReturn)
      case x if TypeSkeleton.getSuperTypes(compiler.compilation)(x).contains(ObjectTypeDelta.rootObjectType) => Seq(AddressReturnInstructionDelta.create)
      case _ => throw new NotImplementedError()
    })
  }

  def getReturnValue[T <: NodeLike](_return: T) = _return(ReturnValue).asInstanceOf[T]

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val expression = find(ExpressionSkeleton.ExpressionGrammar)
    val statement = find(StatementSkeleton.StatementGrammar)

    val returnExpression = "return" ~~> expression.as(ReturnValue) ~< ";" asNode ReturnInteger
    statement.inner = statement.inner | returnExpression
  }

  def _return(value: Node): Node = new Node(ReturnInteger, ReturnValue -> value)

  object ReturnInteger extends NodeClass

  object ReturnValue extends NodeField

  override val key = ReturnInteger

  override def toByteCode(_return: Path, compilation: Compilation): Seq[Node] = {
    val methodCompiler = MethodDelta.getMethodCompiler(compilation)
    returnToLines(_return, methodCompiler)
  }

  override def description: String = "Allows returning a value using an expression."
}
