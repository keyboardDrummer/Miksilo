package transformations.javac.expressions.postfix

import core.transformation.Contract
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.javac.expressions.ExpressionC

object PostFixPrecedenceC extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(ExpressionC)

  override def transformGrammars(grammars: GrammarCatalogue) = {
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    val postFixGrammar = grammars.create(PostFixGrammar, expressionGrammar.inner)
    expressionGrammar.inner = postFixGrammar
  }

  object PostFixGrammar

}
