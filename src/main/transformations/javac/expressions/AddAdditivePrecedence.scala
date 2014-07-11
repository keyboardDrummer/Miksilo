package transformations.javac.expressions

import core.transformation.{GrammarCatalogue, GrammarTransformation, ProgramTransformation}
import transformations.javac.base.JavaBase

object AddAdditivePrecedence extends GrammarTransformation {

  override def dependencies: Set[ProgramTransformation] = Set(JavaBase)

  object AdditiveExpressionGrammar


  override def transformGrammars(grammars: GrammarCatalogue) = {
    val expressionGrammar = grammars.find(AddExpression.ExpressionGrammar)
    val additiveGrammar = grammars.create(AdditiveExpressionGrammar, expressionGrammar.inner)
    expressionGrammar.inner = additiveGrammar
  }
}
