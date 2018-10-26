package deltas.verilog

import core.deltas.{Delta, LanguageFromDeltas, ParseUsingTextualGrammar}
import core.language.Language
import core.smarts.SolveConstraintsDelta
import deltas.expression.IntLiteralDelta
import deltas.expressions.{ExpressionDelta, VariableDelta}
import deltas.javac.trivia.{JavaStyleBlockCommentsDelta, JavaStyleLineCommentsDelta}
import deltas.statement.{ForLoopDelta, _}
import deltas.verilog.preprocessor.{IncludeDelta, PreprocessorDelta}

object VerilogLanguage {
  val genericDeltas: Seq[Delta] = Seq(JavaStyleBlockCommentsDelta, JavaStyleLineCommentsDelta,
    ForLoopDelta, WhileLoopDelta, IfThenElseDelta, IfThenDelta, BlockDelta, StatementDelta,
    IntLiteralDelta, VariableDelta, ExpressionDelta,
    SolveConstraintsDelta)

  val deltas: Seq[Delta] = Seq(
    VerilogWildcardImportDelta,
    IncludeDelta, PreprocessorDelta,
    AlwaysDelta, NonBlockingAssignmentDelta, BeginEndDelta,
    PortTypeSpecifierDelta,
    PackageDelta,
    VerilogClassDelta,
    VerilogModuleDelta,
    VerilogFileDelta) ++ genericDeltas
  val language: Language = LanguageFromDeltas(Seq(ParseUsingTextualGrammar) ++ deltas)
}
