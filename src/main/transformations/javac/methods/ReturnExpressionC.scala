package transformations.javac.methods

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, MetaLike}
import core.particles.path.Path
import transformations.bytecode.coreInstructions.integers.IntegerReturnInstructionC
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.statements.{StatementInstance, StatementSkeleton}

object ReturnExpressionC extends StatementInstance {

  override def dependencies: Set[Contract] = Set(MethodC, IntegerReturnInstructionC)

  override def getNextStatements(obj: Path, labels: Map[Any, Path]): Set[Path] = Set.empty

  def returnToLines(_return: Path, compiler: MethodCompiler): Seq[Node] = {
    val returnValueInstructions = ExpressionSkeleton.getToInstructions(compiler.state)(getReturnValue(_return))
    returnValueInstructions ++ Seq(IntegerReturnInstructionC.integerReturn)
  }

  def getReturnValue[T <: MetaLike](_return: T) = _return(ReturnValue).asInstanceOf[T]

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val expression = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val statement = grammars.find(StatementSkeleton.StatementGrammar)

    val returnExpression = "return" ~~> expression <~ ";" ^^ parseMap(ReturnInteger, ReturnValue)
    statement.inner = statement.inner | returnExpression
  }

  def _return(value: Node): Node = new Node(ReturnInteger, ReturnValue -> value)

  object ReturnInteger

  object ReturnValue

  override val key: AnyRef = ReturnInteger

  override def toByteCode(_return: Path, state: CompilationState): Seq[Node] = {
    val methodCompiler = MethodC.getMethodCompiler(state)
    returnToLines(_return, methodCompiler)
  }

  override def description: String = "Allows returning a value using an expression."
}
