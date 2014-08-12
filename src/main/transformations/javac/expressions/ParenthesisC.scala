package transformations.javac.expressions

import core.transformation.Contract
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation

object ParenthesisC extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(ExpressionC)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val core = grammars.find(ExpressionC.CoreGrammar)
    val expression = grammars.find(ExpressionC.ExpressionGrammar)
    val parseParenthesis = "(" ~> expression <~ ")"
    core.orToInner(parseParenthesis)
  }
}
