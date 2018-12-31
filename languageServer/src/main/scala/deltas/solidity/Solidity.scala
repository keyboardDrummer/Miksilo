package deltas.solidity

import core.deltas.{LanguageFromDeltas, ParseUsingTextualGrammar}
import deltas.expression.IntLiteralDelta
import deltas.expressions.{ExpressionDelta, VariableDelta}
import deltas.javac.CallVariable
import deltas.javac.expressions.additive.{AdditionDelta, AdditivePrecedenceDelta, SubtractionDelta}
import deltas.javac.expressions.relational.{AddRelationalPrecedenceDelta, GreaterThanDelta, GreaterThanOrEqualDelta}
import deltas.javac.methods.{MemberSelectorDelta, ReturnExpressionDelta}
import deltas.javac.methods.assignment._
import deltas.javac.methods.call.CallDelta
import deltas.javac.statements.ExpressionAsStatementDelta
import deltas.javac.trivia.JavaStyleLineCommentsDelta
import deltas.statement.{BlockDelta, IfThenDelta, StatementDelta}

object Solidity {

  private val genericDeltas = Seq(
    JavaStyleLineCommentsDelta,
    CallVariable, CallDelta, MemberSelectorDelta,
    IfThenDelta,
    BlockDelta, ReturnExpressionDelta, ExpressionAsStatementDelta, StatementDelta,
    GreaterThanOrEqualDelta, GreaterThanDelta, AddRelationalPrecedenceDelta,
    DecrementAssignmentDelta, SubtractionDelta,
    IncrementAssignmentDelta, AdditionDelta, AdditivePrecedenceDelta,
    AssignToVariable, VariableDelta, EqualsAssignmentDelta, AssignmentPrecedence,
    IntLiteralDelta,
    ExpressionDelta)

  val deltas = Seq(ParseUsingTextualGrammar, SolidityConstructorDelta, SolidityFunctionDelta, StateVariableDeclarationDelta) ++
    Seq(SolidityContractDelta, ObjectTypeDelta, PragmaDelta) ++
    Seq(MultipleImportsDelta, SingleImportDelta, FileImportDelta) ++
    Seq(SolidityFile, SolidityTypeDelta) ++
    genericDeltas

  val language = LanguageFromDeltas
}
