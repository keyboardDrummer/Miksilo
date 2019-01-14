package deltas.solidity

import core.deltas.path.PathRoot
import core.deltas.{Delta, LanguageFromDeltas, ParseUsingTextualGrammar}
import core.language.Language
import core.smarts.SolveConstraintsDelta
import deltas.ConstraintSkeleton
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
    SolidityLibraryDelta,
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

object SolidityLibraryDelta extends Delta {

  override def inject(language: Language): Unit = {
    language.collectConstraints = (compilation, builder) => {
      val rootScope = builder.newScope(debugName = "rootScope")
      val assertType = SolidityFunctionTypeDelta.createType(compilation, builder, rootScope, Seq(BooleanTypeDelta.booleanType), Seq.empty)
      builder.declare("assert", rootScope, _type = Some(assertType))

      builder.declare("require", rootScope, _type = Some(assertType))

      val revertType = SolidityFunctionTypeDelta.createType(compilation, builder, rootScope, Seq.empty, Seq.empty)
      builder.declare("revert", rootScope, _type = Some(revertType))
      ConstraintSkeleton.constraints(compilation, builder, PathRoot(compilation.program), rootScope)
    }
  }

  override def description = "Adds the solidity standard library"

  override def dependencies = Set.empty
}

