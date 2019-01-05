package deltas.solidity

import core.deltas.{LanguageFromDeltas, ParseUsingTextualGrammar}
import deltas.bytecode.types.{ArrayTypeDelta, TypeSkeleton}
import deltas.expression._
import deltas.expression.relational._
import deltas.javac.CallVariableDelta
import deltas.javac.classes.{AssignToMemberDelta, SelectFieldDelta}
import deltas.javac.expressions.additive.{AdditionDelta, AdditivePrecedenceDelta, SubtractionDelta}
import deltas.javac.methods.assignment._
import deltas.javac.methods.call.CallDelta
import deltas.javac.methods.{MemberSelectorDelta, ReturnExpressionDelta}
import deltas.javac.statements.ExpressionAsStatementDelta
import deltas.statement._
import deltas.trivia.{SlashSlashLineCommentsDelta, SlashStarBlockCommentsDelta}

object SolidityLanguage {

  private val genericDeltas = Seq(
    SlashSlashLineCommentsDelta, SlashStarBlockCommentsDelta,
    LocalDeclarationWithInitializerDelta, LocalDeclarationDelta,
    CallVariableDelta, CallDelta, MemberSelectorDelta,
    ForLoopDelta, BlockAsStatementDelta, WhileLoopDelta, LabelStatementDelta, GotoStatementDelta,
    IfThenDelta,
    BlockDelta, ReturnExpressionDelta, ExpressionAsStatementDelta, StatementDelta,
    EqualsComparisonDelta,
    LessThanDelta,
    GreaterThanOrEqualDelta, GreaterThanDelta, AddRelationalPrecedenceDelta,
    PostFixIncrementDelta,
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
