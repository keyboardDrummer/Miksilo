package transformations.javac.methods

import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.bytecode.coreInstructions.{IntegerReturnC, VoidReturnC}
import transformations.javac.base.{JavaMethodAndClassC, MethodCompiler}
import transformations.javac.expressions.ExpressionC
import transformations.javac.statements.StatementC

object ReturnC extends GrammarTransformation {


  override def dependencies: Set[Contract] = Set(JavaMethodAndClassC, VoidReturnC, IntegerReturnC)

  override def inject(state: TransformationState): Unit = {
    StatementC.getStatementToLines(state).put(Return, (_return: MetaObject) => {
      val methodCompiler = JavaMethodAndClassC.getMethodCompiler(state)
      returnToLines(_return, methodCompiler)
    })
  }

  def returnToLines(_return: MetaObject, compiler: MethodCompiler): Seq[MetaObject] = {
    val mbValue = getReturnValue(_return)
    mbValue match {
      case Some(value) =>
        val returnValueInstructions = ExpressionC.getToInstructions(compiler.transformationState)(value)
        returnValueInstructions ++ Seq(IntegerReturnC.integerReturn)
      case None => Seq(VoidReturnC.voidReturn)
    }
  }

  def getReturnValue(_return: MetaObject) = _return(ReturnValue).asInstanceOf[Option[MetaObject]]

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val expression = grammars.find(ExpressionC.ExpressionGrammar)
    val statement = grammars.find(StatementC.StatementGrammar)

    val returnExpression = "return" ~> expression <~ ";" ^^ (expr => _return(Some(expr.asInstanceOf[MetaObject])))
    statement.inner = statement.inner | returnExpression
  }

  def _return(value: Option[MetaObject] = None): MetaObject = new MetaObject(Return) {
    data.put(ReturnValue, value)
  }

  object Return

  object ReturnValue

}
