package deltas.expressions

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node._

object ExpressionDelta extends DeltaWithGrammar {

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    val core = grammars.create(LastPrecedenceGrammar)
    grammars.create(FirstPrecedenceGrammar, core)
  }

  object LastPrecedenceGrammar extends GrammarKey
  object FirstPrecedenceGrammar extends GrammarKey

  override def description: String = "Introduces the concept of an expression."
}
