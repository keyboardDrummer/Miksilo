package transformations.javac.expressions

import core.particles.grammars.GrammarCatalogue
import core.particles.{Contract, ParticleWithGrammar}

object ParenthesisC extends ParticleWithGrammar {

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val core = grammars.find(ExpressionSkeleton.CoreGrammar)
    val expression = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val parseParenthesis = ("(" ~> produce("(" )) ~> expression <~ ")" //TODO: the produce("(") here is a hack to make sure no () show up when printing. Nicer option would be to have a BiGrammar that specifies parsing/printing behavior.
    core.addOption(parseParenthesis)
  }

  override def description: String = "Allows wrapping an expression in parenthesis to control operator precedence."
}
