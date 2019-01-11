package deltas.solidity

import core.deltas.grammars.LanguageGrammars
import core.deltas.{DeltaWithGrammar, LanguageFromDeltas, ParseUsingTextualGrammar}
import core.language.Language
import deltas.bytecode.types.{ArrayTypeDelta, QualifiedObjectTypeDelta, TypeSkeleton, UnqualifiedObjectTypeDelta}
import deltas.expression._
import deltas.expression.additive.{AdditionDelta, AdditivePrecedenceDelta, SubtractionDelta}
import deltas.expression.prefix.{BitwiseNotDelta, LogicalNotDelta, MinusPrefixOperatorDelta, PlusPrefixOperatorDelta}
import deltas.expression.relational._
import deltas.javac.CallVariableDelta
import deltas.javac.classes.{AssignToMemberDelta, SelectFieldDelta}
import deltas.javac.methods.assignment._
import deltas.javac.methods.call.CallDelta
import deltas.javac.methods.{MemberSelectorDelta, ReturnExpressionDelta}
import deltas.javac.statements.{ExpressionAsStatementDelta, ForLoopContinueDelta, WhileBreakDelta}
import deltas.statement._
import deltas.trivia.{SlashSlashLineCommentsDelta, SlashStarBlockCommentsDelta}

object SolidityLanguage {

  private val genericDeltas = Seq(
    NewDelta, UnqualifiedObjectTypeDelta, QualifiedObjectTypeDelta,
    SlashSlashLineCommentsDelta, SlashStarBlockCommentsDelta,
    ForLoopContinueDelta, ForLoopDelta,
    LocalDeclarationWithInitializerDelta,
    LocalDeclarationDelta,
    CallVariableDelta, CallDelta, MemberSelectorDelta,
    WhileContinueDelta, WhileBreakDelta,
    BlockAsStatementDelta, WhileLoopDelta, LabelStatementDelta, GotoStatementDelta,
    IfThenElseDelta, IfThenDelta,
    BlockDelta, ReturnExpressionDelta, ExpressionAsStatementDelta, StatementDelta,
    EqualsComparisonDelta,
    LessThanDelta,
    GreaterThanOrEqualDelta, GreaterThanDelta, AddRelationalPrecedenceDelta,
    PostFixIncrementDelta, PostFixDecrementDelta,
    SubtractAssignmentDelta, SubtractionDelta,
    AddAssignmentDelta, AdditionDelta, AdditivePrecedenceDelta,
    AssignToMemberDelta, SelectFieldDelta, MemberSelectorDelta,
    AssignToArrayMember,
    AssignToVariable, VariableDelta, SimpleAssignmentDelta, AssignmentPrecedence,
    PlusPrefixOperatorDelta, MinusPrefixOperatorDelta,
    BitwiseNotDelta, LogicalNotDelta, ExponentOperatorDelta,
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