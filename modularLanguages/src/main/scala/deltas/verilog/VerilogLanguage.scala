package deltas.verilog

import core.SolveConstraintsDelta
import core.deltas.{Delta, LanguageFromDeltas, ParseUsingTextualGrammar}
import core.language.Language
import deltas.{FileWithMembersDelta, HasNameDelta}
import deltas.expression.{ExpressionDelta, IntLiteralDelta, VariableDelta}
import deltas.trivia.{SlashSlashLineCommentsDelta, SlashStarBlockCommentsDelta}
import deltas.statement.{ForLoopDelta, _}
import deltas.verilog.preprocessor.{IncludeDelta, PreprocessorDelta}

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
