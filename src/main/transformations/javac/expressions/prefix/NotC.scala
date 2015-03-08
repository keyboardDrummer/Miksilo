package transformations.javac.expressions.prefix

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{MetaObject, CompilationState}
import transformations.bytecode.extraBooleanInstructions.NotInstructionC
import transformations.javac.expressions.{ExpressionSkeleton, ExpressionInstance}
import transformations.types.BooleanTypeC

object NotC extends ExpressionInstance {

  object NotKey

  object NotExpression

  override val key: AnyRef = NotKey

  override def getType(expression: MetaObject, state: CompilationState): MetaObject = BooleanTypeC.booleanType

  override def toByteCode(expression: MetaObject, state: CompilationState): Seq[MetaObject] = {
    ExpressionSkeleton.getToInstructions(state)(expression(NotExpression).asInstanceOf[MetaObject]) ++ Seq(NotInstructionC.not)
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val coreGrammar = grammars.find(ExpressionSkeleton.CoreGrammar)
    coreGrammar.addOption("!" ~> coreGrammar ^^ parseMap(NotKey, NotExpression))
  }

  override def description: String = "Adds the ! (not) operator."
}
