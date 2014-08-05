package transformations.javac.expressions

import core.transformation.Contract
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation

object AddRelationalPrecedence extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(ExpressionC)

  override def transformGrammars(grammars: GrammarCatalogue) = {
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    val relationalGrammar = grammars.create(RelationalExpressionGrammar, expressionGrammar.inner)
    expressionGrammar.inner = relationalGrammar
  }

  object RelationalExpressionGrammar

}
