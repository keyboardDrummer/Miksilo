package deltas.javac.expressions

import core.deltas.grammars.LanguageGrammars
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.Language
import deltas.expression.ExpressionDelta

object ParenthesisInExpressionDelta extends DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val core = find(ExpressionDelta.LastPrecedenceGrammar)
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val parseParenthesis = ("(" ~> value("(" )) ~> expression ~< ")" //TODO: the produce("(") here is a hack to make sure no () show up when printing. Nicer option would be to have a BiGrammar that specifies parsing/printing behavior.
    core.addAlternative(parseParenthesis)
  }

  override def description: String = "Allows wrapping an expression in parenthesis to control operator precedence."
}
