package deltas.smithy

import core.deltas.{Delta, LanguageFromDeltas, ParseUsingTextualGrammar}
import core.language.Language
import core.smarts.SolveConstraintsDelta
import deltas.json.JsonLanguage
import deltas.trivia.SlashSlashLineCommentsDelta
import deltas.{FileWithMembersDelta, HasNameDelta}

object SmithyLanguage {
  val deltas: Seq[Delta] = Seq(SmithyStandardLibrary, SmithyListDelta, StructureStatementDelta,
    OperationDelta, SimpleShapeDelta, ShapeStatementDelta, TraitDelta, ResourceDelta, ServiceDelta,
    GenericSmithyDelta, AbsoluteShapeIdentifierDelta, RelativeShapeIdentifierDelta,
    NamespaceDelta, FileWithMembersDelta, HasNameDelta, SlashSlashLineCommentsDelta, SolveConstraintsDelta) ++
    JsonLanguage.deltas

  val language: Language = LanguageFromDeltas(Seq(ParseUsingTextualGrammar) ++ deltas)
}






