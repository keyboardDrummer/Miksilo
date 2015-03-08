package transformations.javac.expressions

import core.transformation.Contract
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation

object ParenthesisC extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val core = grammars.find(ExpressionSkeleton.CoreGrammar)
    val expression = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val parseParenthesis = "(" ~> expression <~ ")"
    core.addOption(parseParenthesis)
  }

  override def description: String = "Allows wrapping an expression in parenthesis to control operator precedence."
}
