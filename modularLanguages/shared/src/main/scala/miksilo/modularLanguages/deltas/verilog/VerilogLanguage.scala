package miksilo.modularLanguages.deltas.verilog

import miksilo.modularLanguages.core.deltas.{Delta, LanguageFromDeltas, ParseUsingTextualGrammar}
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.core.SolveConstraintsDelta
import miksilo.modularLanguages.deltas.{FileWithMembersDelta, HasNameDelta}
import miksilo.modularLanguages.deltas.expression.{ExpressionDelta, IntLiteralDelta, VariableDelta}
import miksilo.modularLanguages.deltas.trivia.{SlashSlashLineCommentsDelta, SlashStarBlockCommentsDelta}
import miksilo.modularLanguages.deltas.statement.{ForLoopDelta, _}
import miksilo.modularLanguages.deltas.verilog.preprocessor.{IncludeDelta, PreprocessorDelta}

object VerilogLanguage {
  val genericDeltas: Seq[Delta] = Seq(SlashStarBlockCommentsDelta, SlashSlashLineCommentsDelta,
    ForLoopDelta, BlockAsStatementDelta, WhileLoopDelta, LabelStatementDelta, GotoStatementDelta,
    IfThenElseDelta, IfThenDelta, BlockDelta, StatementDelta,
    IntLiteralDelta, VariableDelta, ExpressionDelta,
    HasNameDelta,
    SolveConstraintsDelta)

  val deltas: Seq[Delta] = Seq(
    VerilogWildcardImportDelta,
    IncludeDelta, PreprocessorDelta,
    AlwaysDelta, NonBlockingAssignmentDelta, BeginEndDelta,
    PortTypeSpecifierDelta,
    PackageDelta,
    VerilogClassDelta,
    VerilogModuleDelta,
    FileWithMembersDelta) ++ genericDeltas
  val language: Language = LanguageFromDeltas(Seq(ParseUsingTextualGrammar()) ++ deltas)
}
