package deltas.verilog

import core.deltas.{Delta, ParseUsingTextualGrammar}
import core.language.Language
import core.smarts.SolveConstraintsDelta
import deltas.expression.IntLiteralDelta
import deltas.expressions.{ExpressionDelta, VariableDelta}
import deltas.javac.statements.{ForLoopDelta, WhileLoopDelta}
import deltas.statement.{BlockDelta, IfThenDelta, IfThenElseDelta, StatementDelta}

object VerilogLanguage {
  val genericDelta = Seq(ForLoopDelta, WhileLoopDelta, IfThenElseDelta, IfThenDelta, BlockDelta, StatementDelta,
    IntLiteralDelta, VariableDelta, ExpressionDelta,
    ParseUsingTextualGrammar, SolveConstraintsDelta)

  val deltas: Seq[Delta] = Seq(AlwaysDelta, NonBlockingAssignmentDelta, BeginEndDelta,
    PortTypeSpecifierDelta,
    VerilogModuleDelta,
    ) ++ genericDelta
  val language: Language = Delta.buildLanguage(deltas)
}
