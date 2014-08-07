package transformations.javac.expressions

import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import transformations.bytecode.coreInstructions.IntegerConstantC
import transformations.javac.types.{BooleanTypeC, IntTypeC}

object LiteralC extends ExpressionInstance {
  //TODO split into boolean and integer
  val key = LiteralKey

  def getValue(literal: MetaObject) = {
    literal(ValueKey)
  }

  override def dependencies: Set[Contract] = Set(ExpressionC, IntegerConstantC)

  override def transformGrammars(grammars: GrammarCatalogue) = {
    val parseNumber = number ^^ (number => LiteralC.literal(Integer.parseInt(number.asInstanceOf[String])))
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    expressionGrammar.inner = expressionGrammar.inner | parseNumber
    grammars
  }

  def literal(value: AnyVal) = {
    new MetaObject(LiteralKey) {
      data.put(ValueKey, value)
    }
  }

  object LiteralKey


  object ValueKey

  override def toByteCode(literal: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val value = getValue(literal)
    Seq(value match {
      case i: Integer => IntegerConstantC.integerConstant(i)
      case b: Boolean => IntegerConstantC.integerConstant(if (b) 1 else 0)
    })
  }

  override def getType(expression: MetaObject, state: TransformationState): MetaObject =
    getValue(expression) match {
      case i: Integer => IntTypeC.intType
      case b: Boolean => BooleanTypeC.booleanType
    }
}
