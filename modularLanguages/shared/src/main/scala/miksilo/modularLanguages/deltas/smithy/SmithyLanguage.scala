package miksilo.modularLanguages.deltas.smithy

import core.SolveConstraintsDelta
import miksilo.modularLanguages.core.deltas.{Delta, LanguageFromDeltas, ParseUsingTextualGrammar}
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.deltas.json.ModularJsonLanguage
import miksilo.modularLanguages.deltas.trivia.SlashSlashLineCommentsDelta
import miksilo.modularLanguages.deltas.{FileWithMembersDelta, HasNameDelta}

object SmithyLanguage {
  val deltas: Seq[Delta] = Seq(SmithyStandardLibrary, SmithyListDelta, StructureStatementDelta,
    OperationDelta, SimpleShapeDelta, ShapeStatementDelta, TraitDelta, ResourceDelta, ServiceDelta,
    GenericSmithyDelta, AbsoluteShapeIdentifierDelta, RelativeShapeIdentifierDelta,
    NamespaceDelta, FileWithMembersDelta, HasNameDelta, SlashSlashLineCommentsDelta, SolveConstraintsDelta) ++
    ModularJsonLanguage.deltas

  val language: Language = LanguageFromDeltas(Seq(ParseUsingTextualGrammar()) ++ deltas)
}






