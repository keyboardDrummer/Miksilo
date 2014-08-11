package transformations.javac.methods

import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.bytecode.coreInstructions.IntegerReturnInstructionC
import transformations.javac.base.{MethodAndClassC, MethodCompiler}
import transformations.javac.expressions.ExpressionC
import transformations.javac.statements.StatementC

object ReturnExpressionC extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(MethodAndClassC, IntegerReturnInstructionC)

  override def inject(state: TransformationState): Unit = {
    StatementC.getStatementToLines(state).put(ReturnInteger, (_return: MetaObject) => {
      val methodCompiler = MethodAndClassC.getMethodCompiler(state)
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

    val returnExpression = "return" ~> expression <~ ";" ^^ (expr => _return(expr.asInstanceOf[MetaObject]))
    statement.inner = statement.inner | returnExpression
  }

  def _return(value: MetaObject): MetaObject = new MetaObject(ReturnInteger, ReturnValue -> value)

  object ReturnInteger

  object ReturnValue

}