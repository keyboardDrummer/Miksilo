package transformations.javac.expressions

import core.transformation.{Contract, GrammarCatalogue, GrammarTransformation}

object AddAdditivePrecedence extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(ExpressionC)

  object AdditiveExpressionGrammar


  override def transformGrammars(grammars: GrammarCatalogue) = {
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    val additiveGrammar = grammars.create(AdditiveExpressionGrammar, expressionGrammar.inner)
    expressionGrammar.inner = additiveGrammar
  }
}
