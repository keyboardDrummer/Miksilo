package deltas.smithy

import core.deltas.{Delta, LanguageFromDeltas, ParseUsingTextualGrammar}
import core.language.Language
import deltas.json.JsonLanguage
import deltas.trivia.SlashSlashLineCommentsDelta
import deltas.{FileWithMembersDelta, HasNameDelta}

object SmithyLanguage {
  val deltas: Seq[Delta] = Seq(SmithyListDelta, StructureStatementDelta,
    OperationDelta, SimpleShapeDelta, ShapeStatementDelta, TraitDelta, ServiceOrResourceDelta,
    GenericSmithyDelta, NamespaceDelta, FileWithMembersDelta, HasNameDelta, SlashSlashLineCommentsDelta) ++ JsonLanguage.deltas

  val language: Language = LanguageFromDeltas(Seq(ParseUsingTextualGrammar) ++ deltas)
}






