package deltas.solidity

import core.deltas.{LanguageFromDeltas, ParseUsingTextualGrammar}
import deltas.expressions.ExpressionDelta
import deltas.javac.expressions.additive.AdditionDelta
import deltas.javac.methods.ReturnExpressionDelta
import deltas.javac.methods.assignment.{AssignToVariable, AssignmentDelta, IncrementAssignmentDelta}
import deltas.javac.statements.ExpressionAsStatementDelta
import deltas.statement.{BlockDelta, IfThenDelta, StatementDelta}

object Solidity {

  private val genericDeltas = Seq(
    IfThenDelta,
    BlockDelta, ReturnExpressionDelta, ExpressionAsStatementDelta, StatementDelta,
    IncrementAssignmentDelta, AdditionDelta,
    AssignToVariable, AssignmentDelta, ExpressionDelta)

  val deltas = Seq(ParseUsingTextualGrammar, SolidityConstructorDelta, SolidityFunctionDelta, StateVariableDeclarationDelta) ++
    Seq(ContractDelta, PragmaDelta) ++
    Seq(MultipleImportsDelta, SingleImportDelta, FileImportDelta) ++
    Seq(SolidityFile, SolidityTypeDelta) ++
    genericDeltas

  val language = LanguageFromDeltas
}
