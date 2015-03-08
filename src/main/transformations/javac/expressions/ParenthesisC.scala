package transformations.javac.expressions

import core.transformation.{ParticleWithGrammar, Contract}
import core.transformation.grammars.GrammarCatalogue

object ParenthesisC extends ParticleWithGrammar {

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val core = grammars.find(ExpressionSkeleton.CoreGrammar)
    val expression = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val parseParenthesis = "(" ~> expression <~ ")"
    core.addOption(parseParenthesis)
  }

  override def description: String = "Allows wrapping an expression in parenthesis to control operator precedence."
}
