package transformations.javac.methods

import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.bytecode.coreInstructions.integers.IntegerReturnInstructionC
import transformations.javac.expressions.ExpressionC
import transformations.javac.statements.StatementC

object ReturnExpressionC extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(MethodC, IntegerReturnInstructionC)

  override def inject(state: TransformationState): Unit = {
    StatementC.getStatementToLines(state).put(ReturnInteger, (_return: MetaObject) => {
      val methodCompiler = MethodC.getMethodCompiler(state)
      returnToLines(_return, methodCompiler)
    })
  }

  def returnToLines(_return: MetaObject, compiler: MethodCompiler): Seq[MetaObject] = {
    val returnValueInstructions = ExpressionC.getToInstructions(compiler.transformationState)(getReturnValue(_return))
    returnValueInstructions ++ Seq(IntegerReturnInstructionC.integerReturn)
  }

  def getReturnValue(_return: MetaObject) = _return(ReturnValue).asInstanceOf[MetaObject]

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val expression = grammars.find(ExpressionC.ExpressionGrammar)
    val statement = grammars.find(StatementC.StatementGrammar)

    val returnExpression = "return" ~~> expression <~ ";" ^^ parseMap(ReturnInteger, ReturnValue)
    statement.inner = statement.inner | returnExpression
  }

  def _return(value: MetaObject): MetaObject = new MetaObject(ReturnInteger, ReturnValue -> value)

  object ReturnInteger

  object ReturnValue

}
