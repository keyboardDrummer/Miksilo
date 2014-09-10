package transformations.javac.expressions.prefix

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.extraBooleanInstructions.NotInstructionC
import transformations.javac.expressions.{ExpressionC, ExpressionInstance}
import transformations.types.BooleanTypeC

object NotC extends ExpressionInstance {

  object NotKey

  object NotExpression

  override val key: AnyRef = NotKey

  override def getType(expression: MetaObject, state: TransformationState): MetaObject = BooleanTypeC.booleanType

  override def toByteCode(expression: MetaObject, state: TransformationState): Seq[MetaObject] = {
    ExpressionC.getToInstructions(state)(expression(NotExpression).asInstanceOf[MetaObject]) ++ Seq(NotInstructionC.not)
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val coreGrammar = grammars.find(ExpressionC.CoreGrammar)
    coreGrammar.addOption("!" ~> coreGrammar ^^ parseMap(NotKey, NotExpression))
  }
}
