package transformations.javac.expressions

import core.transformation.{Contract, GrammarCatalogue, GrammarTransformation}

object AddRelationalPrecedence extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(ExpressionC)

  object RelationalExpressionGrammar


  override def transformGrammars(grammars: GrammarCatalogue) = {
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    val relationalGrammar = grammars.create(RelationalExpressionGrammar, expressionGrammar.inner)
    expressionGrammar.inner = relationalGrammar
  }
}
