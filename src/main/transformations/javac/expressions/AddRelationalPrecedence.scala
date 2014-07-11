package transformations.javac.expressions

import core.transformation.{GrammarCatalogue, GrammarTransformation, ProgramTransformation}
import transformations.javac.base.JavaBase

object AddRelationalPrecedence extends GrammarTransformation {

  override def dependencies: Set[ProgramTransformation] = Set(JavaBase)

  object RelationalExpressionGrammar


  override def transformGrammars(grammars: GrammarCatalogue) = {
    val expressionGrammar = grammars.find(AddExpression.ExpressionGrammar)
    val relationalGrammar = grammars.create(RelationalExpressionGrammar, expressionGrammar.inner)
    expressionGrammar.inner = relationalGrammar
  }
}
