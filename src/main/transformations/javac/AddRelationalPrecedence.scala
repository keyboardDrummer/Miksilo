package transformations.javac

import core.grammar.{Grammar, Labelled}
import core.transformation.{ProgramTransformation, GrammarTransformation}
import transformations.javac.base.JavaBase

object AddRelationalPrecedence extends GrammarTransformation {

  override def dependencies: Set[ProgramTransformation] = Set(JavaBase)

  object RelationalExpressionGrammar
  override def transformGrammar(grammar: Grammar): Grammar = {
    val expressionGrammar = grammar.findGrammar(JavaBase.ExpressionGrammar)
    val relationalGrammar = new Labelled(RelationalExpressionGrammar, expressionGrammar.inner)
    expressionGrammar.inner = relationalGrammar

    grammar
  }
}
