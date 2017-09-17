package transformations.javac.methods

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeLike}
import core.particles.path.Path
import transformations.bytecode.coreInstructions.floats.FloatReturnInstructionC
import transformations.bytecode.coreInstructions.integers.IntegerReturnInstructionC
import transformations.bytecode.coreInstructions.longs.LongReturnInstructionC
import transformations.bytecode.coreInstructions.objects.AddressReturnInstructionC
import transformations.bytecode.types._
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.statements.{StatementInstance, StatementSkeleton}

object ReturnExpressionC extends StatementInstance {

  override def dependencies: Set[Contract] = Set(MethodC, IntegerReturnInstructionC)

  override def getNextStatements(obj: Path, labels: Map[Any, Path]): Set[Path] = Set.empty

  def returnToLines(_return: Path, compiler: MethodCompiler): Seq[Node] = {
    val returnValue: Path = getReturnValue(_return)
    val returnValueInstructions = ExpressionSkeleton.getToInstructions(compiler.state)(returnValue)
    val getType = ExpressionSkeleton.getType(compiler.state)
    returnValueInstructions ++ (getType(returnValue) match
    {
      case x if x == IntTypeC.intType => Seq(IntegerReturnInstructionC.integerReturn)
      case x if x == LongTypeC.longType => Seq(LongReturnInstructionC.longReturn)
      case x if x == FloatTypeC.floatType => Seq(FloatReturnInstructionC.create)
      case x if x == DoubleTypeC.doubleType => Seq(LongReturnInstructionC.longReturn)
      case x if TypeSkeleton.getSuperTypes(compiler.state)(x).contains(ObjectTypeC.rootObjectType) => Seq(AddressReturnInstructionC.create)
      case _ => throw new NotImplementedError()
    })
  }

  def getReturnValue[T <: NodeLike](_return: T) = _return(ReturnValue).asInstanceOf[T]

  override def transformGrammars(grammars: GrammarCatalogue, state: CompilationState): Unit = {
    val expression = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val statement = grammars.find(StatementSkeleton.StatementGrammar)

    val returnExpression = "return" ~~> expression <~ ";" asNode(ReturnInteger, ReturnValue)
    statement.inner = statement.inner | returnExpression
  }

  def _return(value: Node): Node = new Node(ReturnInteger, ReturnValue -> value)

  object ReturnInteger extends Key

  object ReturnValue extends Key

  override val key: Key = ReturnInteger

  override def toByteCode(_return: Path, state: CompilationState): Seq[Node] = {
    val methodCompiler = MethodC.getMethodCompiler(state)
    returnToLines(_return, methodCompiler)
  }

  override def description: String = "Allows returning a value using an expression."
}
