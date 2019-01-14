package deltas.solidity

import core.deltas.{LanguageFromDeltas, ParseUsingTextualGrammar}
import core.language.Language
import core.smarts.SolveConstraintsDelta
import deltas.bytecode.types.{ArrayTypeDelta, QualifiedObjectTypeDelta, TypeSkeleton, UnqualifiedObjectTypeDelta}
import deltas.expression._
import deltas.expression.additive.{AdditionDelta, AdditivePrecedenceDelta, SubtractionDelta}
import deltas.expression.bitwise._
import deltas.expression.logical.{LogicalAndDelta, LogicalNotDelta, LogicalOrDelta}
import deltas.expression.multiplicative.{DivideDelta, ModuloDelta, MultiplicativePrecedenceDelta, MultiplyDelta}
import deltas.expression.prefix._
import deltas.expression.relational._
import deltas.javac.CallVariableDelta
import deltas.javac.classes.{AssignToMemberDelta, SelectFieldDelta}
import deltas.javac.methods.call.CallDelta
import deltas.javac.methods.{MemberSelectorDelta, ReturnExpressionDelta}
import deltas.javac.statements.{ExpressionAsStatementDelta, ForLoopContinueDelta, WhileBreakDelta}
import deltas.javac.types.BooleanTypeDelta
import deltas.statement._
import deltas.statement.assignment._
import deltas.trivia.{SlashSlashLineCommentsDelta, SlashStarBlockCommentsDelta}

object SolidityLanguage {

  private val genericDeltas = Seq(
    BitwiseOrAssignmentDelta, BitwiseXorAssignmentDelta, BitwiseAndAssignmentDelta,
    BitwiseShiftLeftAssignmentDelta, BitwiseShiftRightAssignmentDelta,
    MultiplyAssignmentDelta, DivideAssignmentDelta,
    SlashSlashLineCommentsDelta, SlashStarBlockCommentsDelta,
    ForLoopContinueDelta, ForLoopDelta,
    LocalDeclarationWithInitializerDelta,
    LocalDeclarationDelta,
    CallVariableDelta,
    WhileContinueDelta, WhileBreakDelta,
    BlockAsStatementDelta, WhileLoopDelta, LabelStatementDelta, GotoStatementDelta,
    IfThenElseDelta, IfThenDelta,
    BlockDelta, ReturnExpressionDelta, ExpressionAsStatementDelta, StatementDelta,
    PostFixIncrementDelta, PostFixDecrementDelta,
    NewDelta, UnqualifiedObjectTypeDelta, QualifiedObjectTypeDelta,
    CallDelta, MemberSelectorDelta,
    PrefixIncrementDelta, PrefixDecrementDelta,
    PlusPrefixOperatorDelta, MinusPrefixOperatorDelta,
    LogicalNotDelta,
    BitwiseNotDelta,
    ExponentOperatorDelta,
    MultiplyDelta, DivideDelta, ModuloDelta, MultiplicativePrecedenceDelta,
    SubtractAssignmentDelta, SubtractionDelta,
    AddAssignmentDelta, AdditionDelta, AdditivePrecedenceDelta,
    BitwiseShiftLeftDelta, BitwiseShiftRightDelta, BitwiseAndDelta, BitwiseXorDelta, BitwiseOrDelta,
    LessThanDelta, GreaterThanOrEqualDelta, GreaterThanDelta,
    EqualsComparisonDelta, AddRelationalPrecedenceDelta,
    LogicalAndDelta, LogicalOrDelta,
    TernaryDelta,
    AssignToMemberDelta, SelectFieldDelta, MemberSelectorDelta,
    AssignToArrayMember,
    AssignToVariable, VariableDelta, SimpleAssignmentDelta, AssignmentPrecedence,
    ArrayAccessDelta, ArrayLiteralDelta, IntLiteralDelta,
    ParenthesisInExpressionDelta, ExpressionDelta,
    BooleanTypeDelta,
    FixedSizeArrayTypeDelta, ArrayTypeDelta, TypeSkeleton,
    SolveConstraintsDelta)

  val soliditySpecificDeltas = Seq(ParseUsingTextualGrammar,
    AfterOrDeleteExpressionDelta,
    SolidityFunctionTypeDelta,
    MappingTypeDelta,
    InlineAssemblyStatementDelta,
    LocalDeclarationStorageLocationDelta,
    NumberLiteralUnitsDelta,
    EmitStatementDelta,
    UsingForDeclarationDelta, EventDelta, CustomModifierDelta, EnumDelta, StructDelta,
    SolidityConstructorDelta, SolidityFunctionDelta, StateVariableDeclarationDelta) ++
    Seq(SolidityContractDelta, PragmaDelta) ++
    Seq(MultipleImportsDelta, SingleImportDelta, FileImportDelta) ++
    Seq(FileWithMembersDelta) ++
    Seq(ElementaryTypeDelta, StorageLocationDelta, StateMutabilityDelta)

  val deltas = soliditySpecificDeltas ++ genericDeltas

  val language: Language = LanguageFromDeltas(deltas)
}

