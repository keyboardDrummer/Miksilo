package miksilo.modularLanguages.deltas.solidity

import miksilo.modularLanguages.core.deltas._
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.core.SolveConstraintsDelta
import miksilo.modularLanguages.deltas.HasNameDelta
import miksilo.modularLanguages.deltas.bytecode.types.{ArrayTypeDelta, QualifiedObjectTypeDelta, TypeSkeleton, UnqualifiedObjectTypeDelta}
import miksilo.modularLanguages.deltas.classes.constructor.DefaultConstructorDelta
import miksilo.modularLanguages.deltas.expression._
import miksilo.modularLanguages.deltas.expression.additive.{AdditionDelta, AdditivePrecedenceDelta, SubtractionDelta}
import miksilo.modularLanguages.deltas.expression.bitwise._
import miksilo.modularLanguages.deltas.expression.logical.{LogicalAndDelta, LogicalNotDelta, LogicalOrDelta}
import miksilo.modularLanguages.deltas.expression.multiplicative.{DivideDelta, ModuloDelta, MultiplicativePrecedenceDelta, MultiplyDelta}
import miksilo.modularLanguages.deltas.expression.prefix._
import miksilo.modularLanguages.deltas.expression.relational._
import miksilo.modularLanguages.deltas.javac.CallVariableDelta
import miksilo.modularLanguages.deltas.javac.classes.{AssignToMemberDelta, SelectFieldDelta}
import miksilo.modularLanguages.deltas.javac.expressions.literals.BooleanLiteralDelta
import miksilo.modularLanguages.deltas.javac.methods.{MemberSelectorDelta, ReturnExpressionDelta}
import miksilo.modularLanguages.deltas.javac.methods.call.CallMemberDelta
import miksilo.modularLanguages.deltas.javac.statements.{ExpressionAsStatementDelta, ForLoopContinueDelta, WhileBreakDelta}
import miksilo.modularLanguages.deltas.javac.types.BooleanTypeDelta
import miksilo.modularLanguages.deltas.method.call.CallDelta
import miksilo.modularLanguages.deltas.statement._
import miksilo.modularLanguages.deltas.statement.assignment._
import miksilo.modularLanguages.deltas.trivia.{SlashSlashLineCommentsDelta, SlashStarBlockCommentsDelta}

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

  val soliditySpecificDeltas = Seq(ParseUsingTextualGrammar(),
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
