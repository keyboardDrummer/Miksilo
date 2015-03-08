package transformations.javac.methods

import core.particles._
import core.particles.grammars.GrammarCatalogue
import transformations.bytecode.coreInstructions.integers.IntegerReturnInstructionC
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.statements.{StatementSkeleton, StatementInstance}

object ReturnExpressionC extends StatementInstance {

  override def dependencies: Set[Contract] = Set(MethodC, IntegerReturnInstructionC)

  def returnToLines(_return: MetaObject, compiler: MethodCompiler): Seq[MetaObject] = {
    val returnValueInstructions = ExpressionSkeleton.getToInstructions(compiler.transformationState)(getReturnValue(_return))
    returnValueInstructions ++ Seq(IntegerReturnInstructionC.integerReturn)
  }

  def getReturnValue(_return: MetaObject) = _return(ReturnValue).asInstanceOf[MetaObject]

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val expression = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val statement = grammars.find(StatementSkeleton.StatementGrammar)

    val returnExpression = "return" ~~> expression <~ ";" ^^ parseMap(ReturnInteger, ReturnValue)
    statement.inner = statement.inner | returnExpression
  }

  def _return(value: MetaObject): MetaObject = new MetaObject(ReturnInteger, ReturnValue -> value)

  object ReturnInteger

  object ReturnValue

  override val key: AnyRef = ReturnInteger

  override def toByteCode(_return: MetaObject, state: CompilationState): Seq[MetaObject] = {
    val methodCompiler = MethodC.getMethodCompiler(state)
    returnToLines(_return, methodCompiler)
  }

  override def description: String = "Allows returning a value using an expression."
}
