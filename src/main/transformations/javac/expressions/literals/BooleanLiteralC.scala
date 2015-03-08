package transformations.javac.expressions.literals

import core.particles._
import core.particles.grammars.GrammarCatalogue
import transformations.bytecode.coreInstructions.integers.IntegerConstantC
import transformations.javac.expressions.{ExpressionSkeleton, ExpressionInstance}
import transformations.types.BooleanTypeC

object BooleanLiteralC extends ExpressionInstance {
  val key = LiteralBooleanKey

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton, IntegerConstantC)

  override def transformGrammars(grammars: GrammarCatalogue) = {
    val parseNumber = "true" ~> produce(literal(true)) | "false" ~> produce(literal(false))
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    expressionGrammar.inner = expressionGrammar.inner | parseNumber
  }

  def literal(value: Boolean) = new MetaObject(LiteralBooleanKey, ValueKey -> value)

  override def toByteCode(literal: MetaObject, state: CompilationState): Seq[MetaObject] = {
    Seq(IntegerConstantC.integerConstant(if (getValue(literal)) 1 else 0))
  }

  def getValue(literal: MetaObject) = literal(ValueKey).asInstanceOf[Boolean]

  override def getType(expression: MetaObject, state: CompilationState): MetaObject = BooleanTypeC.booleanType

  object LiteralBooleanKey

  object ValueKey

  override def description: String = "Adds the boolean literals 'true' and 'false'"
}
