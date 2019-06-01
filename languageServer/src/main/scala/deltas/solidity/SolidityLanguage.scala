package deltas.solidity

import core.deltas._
import core.language.Language
import core.smarts.SolveConstraintsDelta
import deltas.HasNameDelta
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
import deltas.javac.constructor.DefaultConstructorDelta
import deltas.javac.expressions.literals.BooleanLiteralDelta
import deltas.javac.methods.call.{CallDelta, CallMemberDelta}
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
    CallMemberDelta,
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
    EqualsComparisonDelta, RelationalPrecedenceDelta,
    LogicalAndDelta, LogicalOrDelta,
    TernaryDelta,
    AssignToMemberDelta, SelectFieldDelta, MemberSelectorDelta,
    AssignToArrayMember,
    AssignToVariable, VariableDelta, SimpleAssignmentDelta, AssignmentPrecedence,
    ArrayAccessDelta,
    BracketAccessDelta, ArrayLiteralDelta, IntLiteralDelta, BooleanLiteralDelta,
    ParenthesisInExpressionDelta, ExpressionDelta,
    BooleanTypeDelta,
    FixedSizeArrayTypeDelta, ArrayTypeDelta, TypeSkeleton,
    MultiFileDelta,
    HasNameDelta,
    SolveConstraintsDelta)

  val soliditySpecificDeltas = Seq(ParseUsingTextualGrammar,
    MappingAccessDelta,
    SolidityLibraryDelta,
    AfterOrDeleteExpressionDelta,
    SolidityFunctionTypeDelta,
    MappingTypeDelta,
    SolidityIntLiteralDelta,
    InlineAssemblyStatementDelta,
    LocalDeclarationStorageLocationDelta,
    NumberLiteralUnitsDelta,
    EmitStatementDelta,
    UsingForForElementaryTypesDelta, UsingForDeclarationDelta,
    EventDelta, CustomModifierDelta, EnumDelta, StructDelta,
    DefaultConstructorDelta, SolidityConstructorDelta, SolidityFunctionDelta, StateVariableDeclarationDelta) ++
    Seq(SolidityContractDelta, PragmaDelta) ++
    Seq(MultipleImportsDelta, SingleImportDelta, FileImportDelta) ++
    Seq(FileWithMembersDelta) ++
    Seq(ElementaryTypeDelta, StorageLocationDelta, StateMutabilityDelta)

  val deltas = soliditySpecificDeltas ++ genericDeltas

  val language: Language = LanguageFromDeltas(deltas)
}
