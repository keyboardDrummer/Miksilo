package deltas.smithy

import core.bigrammar.grammars.Keyword
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import deltas.expression.ExpressionDelta
import deltas.{FileWithMembersDelta, HasNameDelta}

object ServiceOrResourceDelta extends DeltaWithGrammar {
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val grammar = (Keyword("service", reserved = false) | Keyword("resource", reserved = false)) ~~ identifier.as(HasNameDelta.Name) ~~ expression
    val members = find(FileWithMembersDelta.Members)
    members.addAlternative(grammar)
  }

  override def description = "Adds the namespace service statement"

  override def dependencies = Set(FileWithMembersDelta)
}
