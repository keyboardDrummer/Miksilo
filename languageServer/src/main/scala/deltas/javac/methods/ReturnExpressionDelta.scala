package deltas.javac.methods

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.bytecode.coreInstructions.floats.FloatReturnInstructionDelta
import deltas.bytecode.coreInstructions.integers.IntegerReturnInstructionDelta
import deltas.bytecode.coreInstructions.longs.LongReturnInstructionDelta
import deltas.bytecode.coreInstructions.objects.AddressReturnInstructionDelta
import deltas.bytecode.types._
import deltas.expressions.ExpressionDelta
import deltas.javac.expressions.ToByteCodeSkeleton
import deltas.javac.statements.ByteCodeStatementInstance
import deltas.statement.{StatementDelta, StatementInstance}

object ReturnExpressionDelta extends ByteCodeStatementInstance with StatementInstance {

  override def description: String = "Allows returning a value using an expression."

  override def dependencies: Set[Contract] = Set(MethodDelta, IntegerReturnInstructionDelta)

  override def getNextStatements(language: Language, obj: NodePath, labels: Map[Any, NodePath]): Set[NodePath] = Set.empty

  def returnToLines(_return: NodePath, compiler: MethodCompiler): Seq[Node] = {
    val returnValue: NodePath = getReturnValue(_return)
    val returnValueInstructions = ToByteCodeSkeleton.getToInstructions(compiler.compilation)(returnValue)
    val getType = ExpressionDelta.getType(compiler.compilation)
    returnValueInstructions ++ (getType(returnValue) match
    {
      case x if x == IntTypeDelta.intType => Seq(IntegerReturnInstructionDelta.integerReturn)
      case x if x == LongTypeDelta.longType => Seq(LongReturnInstructionDelta.longReturn)
      case x if x == FloatTypeDelta.floatType => Seq(FloatReturnInstructionDelta.create)
      case x if x == DoubleTypeDelta.doubleType => Seq(LongReturnInstructionDelta.longReturn)
      case x if TypeSkeleton.getSuperTypes(compiler.compilation)(x).
        contains(QualifiedObjectTypeDelta.rootObjectType) => Seq(AddressReturnInstructionDelta.create)
      case _ => throw new NotImplementedError()
    })
  }

  def getReturnValue[T <: NodeLike](_return: T) = _return(ReturnValue).asInstanceOf[T]

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val statement = find(StatementDelta.Grammar)

    val returnExpression = "return" ~~> expression.as(ReturnValue) ~< ";" asNode ReturnInteger
    statement.addAlternative(returnExpression)
  }

  def _return(value: Node): Node = new Node(ReturnInteger, ReturnValue -> value)

  object ReturnInteger extends NodeShape

  object ReturnValue extends NodeField

  override val shape = ReturnInteger

  override def toByteCode(_return: NodePath, compilation: Compilation): Seq[Node] = {
    val methodCompiler = MethodDelta.getMethodCompiler(compilation)
    returnToLines(_return, methodCompiler)
  }

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, statement: NodePath, parentScope: Scope): Unit = {
    ExpressionDelta.getType(compilation, builder, getReturnValue(statement), parentScope)
  }
}
