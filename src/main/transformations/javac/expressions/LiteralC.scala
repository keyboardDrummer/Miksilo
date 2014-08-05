package transformations.javac.expressions

import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.bytecode.instructions.IntegerConstantC

object LiteralC extends GrammarTransformation {
  override def inject(state: TransformationState): Unit = {
    ExpressionC.getExpressionToLines(state).put(LiteralKey, literal => {
      val value = getValue(literal)
      Seq(value match {
        case i: Integer => IntegerConstantC.integerConstant(i)
        case b: Boolean => IntegerConstantC.integerConstant(if (b) 1 else 0)
      })
    })
  }

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

}
