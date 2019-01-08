package deltas.solidity

import core.deltas.{LanguageFromDeltas, ParseUsingTextualGrammar}
import deltas.bytecode.types.{ArrayTypeDelta, QualifiedObjectTypeDelta, TypeSkeleton, UnqualifiedObjectTypeDelta}
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
    NewDelta, UnqualifiedObjectTypeDelta, QualifiedObjectTypeDelta,
    SlashSlashLineCommentsDelta, SlashStarBlockCommentsDelta,
    ForLoopDelta,
    LocalDeclarationWithInitializerDelta,
    LocalDeclarationDelta,
    CallVariableDelta, CallDelta, MemberSelectorDelta,
    BlockAsStatementDelta, WhileLoopDelta, LabelStatementDelta, GotoStatementDelta,
    IfThenElseDelta, IfThenDelta,
    BlockDelta, ReturnExpressionDelta, ExpressionAsStatementDelta, StatementDelta,
    EqualsComparisonDelta,
    LessThanDelta,
    GreaterThanOrEqualDelta, GreaterThanDelta, AddRelationalPrecedenceDelta,
    PostFixIncrementDelta,
    DecrementAssignmentDelta, SubtractionDelta,
    IncrementAssignmentDelta, AdditionDelta, AdditivePrecedenceDelta,
    AssignToMemberDelta, SelectFieldDelta, MemberSelectorDelta,
    AssignToArrayMember,
    AssignToVariable, VariableDelta, SimpleAssignmentDelta, AssignmentPrecedence,
    ArrayAccessDelta, ArrayLiteralDelta, IntLiteralDelta,
    ExpressionDelta,
    FixedSizeArrayTypeDelta, ArrayTypeDelta, TypeSkeleton)

  val soliditySpecificDeltas = Seq(ParseUsingTextualGrammar,
    InlineAssemblyStatementDelta,
    LocalDeclarationStorageLocationDelta,
    NumberLiteralUnitsDelta,
    EmitStatementDelta,
    UsingForDeclarationDelta, EventDelta, CustomModifierDelta, EnumDelta, StructDelta,
    SolidityConstructorDelta, SolidityFunctionDelta, StateVariableDeclarationDelta) ++
    Seq(SolidityContractDelta, PragmaDelta) ++
    Seq(MultipleImportsDelta, SingleImportDelta, FileImportDelta) ++
    Seq(SolidityFile) ++
    Seq(ElementaryTypeDelta, StorageLocationDelta, StateMutabilityDelta)

  val deltas = soliditySpecificDeltas ++ genericDeltas

  val language = LanguageFromDeltas
}


