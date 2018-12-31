package deltas.solidity

import core.deltas.{LanguageFromDeltas, ParseUsingTextualGrammar}
import deltas.expressions.{ExpressionDelta, VariableDelta}
import deltas.javac.CallVariable
import deltas.javac.expressions.additive.{AdditionDelta, AdditivePrecedenceDelta, SubtractionDelta}
import deltas.javac.methods.{MemberSelectorDelta, ReturnExpressionDelta}
import deltas.javac.methods.assignment._
import deltas.javac.methods.call.CallDelta
import deltas.javac.statements.ExpressionAsStatementDelta
import deltas.statement.{BlockDelta, IfThenDelta, StatementDelta}

object Solidity {

  private val genericDeltas = Seq(
    CallVariable, CallDelta, MemberSelectorDelta,
    IfThenDelta,
    BlockDelta, ReturnExpressionDelta, ExpressionAsStatementDelta, StatementDelta,
    DecrementAssignmentDelta, SubtractionDelta,
    IncrementAssignmentDelta, AdditionDelta, AdditivePrecedenceDelta,
    AssignToVariable, VariableDelta, EqualsAssignmentDelta, AssignmentPrecedence, ExpressionDelta)

  val deltas = Seq(ParseUsingTextualGrammar, SolidityConstructorDelta, SolidityFunctionDelta, StateVariableDeclarationDelta) ++
    Seq(SolidityContractDelta, ObjectTypeDelta, PragmaDelta) ++
    Seq(MultipleImportsDelta, SingleImportDelta, FileImportDelta) ++
    Seq(SolidityFile, SolidityTypeDelta) ++
    genericDeltas

  val language = LanguageFromDeltas
}
