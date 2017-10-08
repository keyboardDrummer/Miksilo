package transformations.javac.methods

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node._
import core.particles.path.Path
import transformations.bytecode.coreInstructions.floats.FloatReturnInstructionDelta
import transformations.bytecode.coreInstructions.integers.IntegerReturnInstructionDelta
import transformations.bytecode.coreInstructions.longs.LongReturnInstructionDelta
import transformations.bytecode.coreInstructions.objects.AddressReturnInstructionDelta
import transformations.bytecode.types._
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.statements.{StatementInstance, StatementSkeleton}

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

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val expression = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val statement = grammars.find(StatementSkeleton.StatementGrammar)

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
