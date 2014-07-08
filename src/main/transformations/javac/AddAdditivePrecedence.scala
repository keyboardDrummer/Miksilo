package transformations.javac

import core.grammar.{Grammar, Labelled}
import core.transformation.{ProgramTransformation, GrammarTransformation}
import transformations.javac.base.JavaBase

object AddAdditivePrecedence extends GrammarTransformation {

  override def dependencies: Set[ProgramTransformation] = Set(JavaBase)

  object AdditiveExpressionGrammar
  override def transformGrammar(grammar: Grammar): Grammar = {
    val expressionGrammar = grammar.findGrammar(JavaBase.ExpressionGrammar)
    val additiveGrammar = new Labelled(AdditiveExpressionGrammar, expressionGrammar.inner)
    expressionGrammar.inner = additiveGrammar
    grammar
  }
}
