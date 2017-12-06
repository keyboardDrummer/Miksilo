package deltas.javac.statements

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.deltas.path.{Path, SequenceElement}
import deltas.bytecode.ByteCodeMethodInfo
import deltas.bytecode.simpleBytecode.{InferredStackFrames, LabelDelta, LabelledLocations}
import deltas.javac.expressions.ExpressionSkeleton

object IfThenDelta extends StatementInstance {

  def neww(condition: Node, thenBody: Seq[Node]): Node = key.create(Condition -> condition, Then -> thenBody)

  object IfThenKey extends NodeClass

  object Condition extends NodeField

  object Then extends NodeField

  override val key = IfThenKey

  override def dependencies: Set[Contract] = super.dependencies ++ Set(BlockDelta)

  override def toByteCode(ifThen: Path, compilation: Compilation): Seq[Node] = {
    val condition = getCondition(ifThen)
    val method = ifThen.findAncestorClass(ByteCodeMethodInfo.MethodInfoKey)
    val endLabelName = LabelDelta.getUniqueLabel("end", method)
    val end = InferredStackFrames.label(endLabelName)
    val body = getThenStatements(ifThen)

    val jumpToEndIfFalse = LabelledLocations.ifZero(endLabelName)
    val toInstructionsExpr = ExpressionSkeleton.getToInstructions(compilation)
    val toInstructionsStatement = StatementSkeleton.getToInstructions(compilation)
    toInstructionsExpr(condition) ++
      Seq(jumpToEndIfFalse) ++
      body.flatMap(toInstructionsStatement) ++
      Seq(end)
  }

  def getCondition[T <: NodeLike](ifThen: T): T = {
    ifThen(Condition).asInstanceOf[T]
  }

  def getThenStatements[T <: NodeLike](ifThen: T): Seq[T] = {
    ifThen(Then).asInstanceOf[Seq[T]]
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statementGrammar = find(StatementSkeleton.StatementGrammar)
    val expressionGrammar = find(ExpressionSkeleton.ExpressionGrammar)
    val bodyGrammar = find(BlockDelta.BlockOrStatementGrammar)
    val ifThenGrammar = create(IfThenKey, ("if" ~> ("(" ~> expressionGrammar.as(Condition) ~< ")") ~ bodyGrammar.as(Then)).
      asNode(IfThenKey))
    statementGrammar.addOption(ifThenGrammar)
  }

  override def description: String = "Enables using the if-then (no else) construct."

  override def getNextStatements(obj: Path, labels: Map[Any, Path]): Set[Path] =
  {
    Set(getThenStatements(obj).head) ++ super.getNextStatements(obj, labels)
  }

  override def getLabels(obj: Path): Map[Any, Path] = {
    val next = obj.asInstanceOf[SequenceElement].next //TODO this will not work for an if-if nesting. Should generate a next label for each statement. But this also requires labels referencing other labels.
    Map(IfThenDelta.getNextLabel(getThenStatements(obj).last) -> next) ++
      super.getLabels(obj)
  }
}
