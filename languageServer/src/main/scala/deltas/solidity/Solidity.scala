package deltas.solidity

import core.deltas.{LanguageFromDeltas, ParseUsingTextualGrammar}
import deltas.expressions.ExpressionDelta
import deltas.javac.methods.ReturnExpressionDelta
import deltas.javac.methods.assignment.{AssignToVariable, AssignmentDelta}
import deltas.javac.statements.ExpressionAsStatementDelta
import deltas.statement.{BlockDelta, StatementDelta}

object Solidity {

  private val genericDeltas = Seq(
    BlockDelta, ReturnExpressionDelta, ExpressionAsStatementDelta, StatementDelta,
    AssignToVariable, AssignmentDelta, ExpressionDelta)

  val deltas = Seq(ParseUsingTextualGrammar, SolidityFunctionDelta, StateVariableDeclarationDelta) ++
    Seq(ContractDelta, PragmaDelta) ++
    Seq(MultipleImportsDelta, SingleImportDelta, FileImportDelta) ++
    Seq(SolidityFile, SolidityTypeDelta) ++
    genericDeltas

  val language = LanguageFromDeltas
}
