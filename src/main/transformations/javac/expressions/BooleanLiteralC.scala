package transformations.javac.expressions

import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import transformations.bytecode.coreInstructions.IntegerConstantC
import transformations.types.BooleanTypeC

object BooleanLiteralC extends ExpressionInstance {
  val key = LiteralBooleanKey

  override def dependencies: Set[Contract] = Set(ExpressionC, IntegerConstantC)

  override def transformGrammars(grammars: GrammarCatalogue) = {
    val parseNumber = "true" ^^ (_ => literal(true)) | "false" ^^ (_ => literal(false))
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    expressionGrammar.inner = expressionGrammar.inner | parseNumber
  }

  def literal(value: Boolean) = new MetaObject(LiteralBooleanKey, ValueKey -> value)

  override def toByteCode(literal: MetaObject, state: TransformationState): Seq[MetaObject] = {
    Seq(IntegerConstantC.integerConstant(if (getValue(literal)) 1 else 0))
  }

  def getValue(literal: MetaObject) = literal(ValueKey).asInstanceOf[Boolean]

  override def getType(expression: MetaObject, state: TransformationState): MetaObject = BooleanTypeC.booleanType

  object LiteralBooleanKey

  object ValueKey

}
