package transformations.javac.expressions

import core.transformation._
import transformations.bytecode.ByteCode

object LiteralC extends GrammarTransformation {
  def literal(value: AnyVal) = {
    new MetaObject(LiteralKey) {
      data.put(ValueKey, value)
    }
  }

  def getValue(literal: MetaObject) = {
    literal(ValueKey)
  }

  object LiteralKey

  object ValueKey

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    ExpressionC.getExpressionToLines(state).put(LiteralKey, literal => {
      val value = getValue(literal)
      Seq(value match {
        case i: Integer => ByteCode.integerConstant(i)
        case b: Boolean => ByteCode.integerConstant(if (b) 1 else 0)
      })
    })
  }

  override def dependencies: Set[ProgramTransformation] = Set(ExpressionC)


  override def transformGrammars(grammars: GrammarCatalogue) = {
    val parseNumber = number ^^ (number => LiteralC.literal(Integer.parseInt(number.asInstanceOf[String])))
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    expressionGrammar.inner = expressionGrammar.inner | parseNumber
    grammars
  }
}
