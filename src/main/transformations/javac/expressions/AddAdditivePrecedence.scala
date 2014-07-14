package transformations.javac.expressions

import core.transformation.{GrammarCatalogue, GrammarTransformation, ProgramTransformation}

object AddAdditivePrecedence extends GrammarTransformation {

  override def dependencies: Set[ProgramTransformation] = Set(ExpressionC)

  object AdditiveExpressionGrammar


  override def transformGrammars(grammars: GrammarCatalogue) = {
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    val additiveGrammar = grammars.create(AdditiveExpressionGrammar, expressionGrammar.inner)
    expressionGrammar.inner = additiveGrammar
  }
}
