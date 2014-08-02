package transformations.javac.expressions

import core.transformation.{Contract, GrammarCatalogue, GrammarTransformation}

object AddEqualityPrecedence extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(ExpressionC)

  override def transformGrammars(grammars: GrammarCatalogue) = {
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    val equalityGrammar = grammars.create(EqualityExpressionGrammar, expressionGrammar.inner)
    expressionGrammar.inner = equalityGrammar
  }

  object EqualityExpressionGrammar

}
