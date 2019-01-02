package deltas.solidity

import core.deltas.{LanguageFromDeltas, ParseUsingTextualGrammar}
import deltas.bytecode.types.{ArrayTypeDelta, TypeSkeleton}
import deltas.expression._
import deltas.javac.CallVariableDelta
import deltas.javac.classes.{AssignToMemberDelta, SelectFieldDelta}
import deltas.javac.expressions.additive.{AdditionDelta, AdditivePrecedenceDelta, SubtractionDelta}
import deltas.javac.expressions.relational.{AddRelationalPrecedenceDelta, EqualsComparisonDelta, GreaterThanDelta, GreaterThanOrEqualDelta}
import deltas.javac.methods.assignment._
import deltas.javac.methods.call.CallDelta
import deltas.javac.methods.{MemberSelectorDelta, ReturnExpressionDelta}
import deltas.javac.statements.ExpressionAsStatementDelta
import deltas.javac.trivia.SlashSlashLineCommentsDelta
import deltas.statement._

object SolidityLanguage {

  private val genericDeltas = Seq(
    SlashSlashLineCommentsDelta,
    LocalDeclarationWithInitializerDelta, LocalDeclarationDelta,
    CallVariableDelta, CallDelta, MemberSelectorDelta,
    IfThenDelta,
    BlockDelta, ReturnExpressionDelta, ExpressionAsStatementDelta, StatementDelta,
    EqualsComparisonDelta,
    GreaterThanOrEqualDelta, GreaterThanDelta, AddRelationalPrecedenceDelta,
    DecrementAssignmentDelta, SubtractionDelta,
    IncrementAssignmentDelta, AdditionDelta, AdditivePrecedenceDelta,
    AssignToMemberDelta, SelectFieldDelta, MemberSelectorDelta,
    AssignToVariable, VariableDelta, EqualsAssignmentDelta, AssignmentPrecedence,
    ArrayAccessDelta, ArrayLiteralDelta, IntLiteralDelta,
    ExpressionDelta,
    FixedSizeArrayTypeDelta, ArrayTypeDelta, ObjectTypeDelta, TypeSkeleton)

  val soliditySpecificDeltas = Seq(ParseUsingTextualGrammar,
    NumberLiteralUnitsDelta,
    EmitStatementDelta,
    UsingForDeclarationDelta, EventDelta, CustomModifierDelta, EnumDelta, StructDelta,
    SolidityConstructorDelta, SolidityFunctionDelta, StateVariableDeclarationDelta) ++
    Seq(SolidityContractDelta, PragmaDelta) ++
    Seq(MultipleImportsDelta, SingleImportDelta, FileImportDelta) ++
    Seq(SolidityFile) ++
    Seq(ElementaryTypeDelta)

  val deltas = soliditySpecificDeltas ++ genericDeltas

  val language = LanguageFromDeltas
}
