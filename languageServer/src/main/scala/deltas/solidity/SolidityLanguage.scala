package deltas.solidity

import core.deltas.grammars.LanguageGrammars
import core.deltas.{DeltaWithGrammar, LanguageFromDeltas, ParseUsingTextualGrammar}
import core.language.Language
import deltas.bytecode.types.{ArrayTypeDelta, QualifiedObjectTypeDelta, TypeSkeleton, UnqualifiedObjectTypeDelta}
import deltas.expression._
import deltas.expression.additive.{AdditionDelta, AdditivePrecedenceDelta, SubtractionDelta}
import deltas.expression.bitwise._
import deltas.expression.multiplicative.{DivideDelta, ModuloDelta, MultiplicativePrecedenceDelta, MultiplyDelta}
import deltas.expression.prefix._
import deltas.expression.relational._
import deltas.javac.CallVariableDelta
import deltas.javac.classes.{AssignToMemberDelta, SelectFieldDelta}
import deltas.javac.methods.call.CallDelta
import deltas.javac.methods.{MemberSelectorDelta, ReturnExpressionDelta}
import deltas.javac.statements.{ExpressionAsStatementDelta, ForLoopContinueDelta, WhileBreakDelta}
import deltas.statement._
import deltas.statement.assignment._
import deltas.trivia.{SlashSlashLineCommentsDelta, SlashStarBlockCommentsDelta}

object SolidityLanguage {

  private val genericDeltas = Seq(
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
    AssignToMemberDelta, SelectFieldDelta, MemberSelectorDelta,
    AssignToArrayMember,
    AssignToVariable, VariableDelta, SimpleAssignmentDelta, AssignmentPrecedence,
    BitwiseNotDelta,
    ArrayAccessDelta, ArrayLiteralDelta, IntLiteralDelta,
    ParenthesisInExpressionDelta, ExpressionDelta,
    FixedSizeArrayTypeDelta, ArrayTypeDelta, TypeSkeleton)

  val soliditySpecificDeltas = Seq(ParseUsingTextualGrammar,
    AfterOrDeleteExpressionDelta,
    MappingTypeDelta,
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

object AfterOrDeleteExpressionDelta extends DeltaWithGrammar {
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val grammar = ("after" | "delete") ~~ expression
    expression.addAlternative(grammar)
  }

  override def description = "Add after and delete expression"

  override def dependencies = Set(ExpressionDelta)
}