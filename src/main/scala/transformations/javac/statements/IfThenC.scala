package transformations.javac.statements

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node._
import core.particles.path.{Path, SequenceElement}
import transformations.bytecode.ByteCodeMethodInfo
import transformations.bytecode.additions.LabelledLocations
import transformations.bytecode.simpleBytecode.InferredStackFrames
import transformations.javac.expressions.ExpressionSkeleton

object IfThenC extends StatementInstance {

  object IfThenKey extends NodeClass

  object Condition extends NodeField

  object Then extends NodeField

  override val key = IfThenKey

  override def dependencies: Set[Contract] = super.dependencies ++ Set(BlockC)

  override def toByteCode(ifThen: Path, compilation: Compilation): Seq[Node] = {
    val condition = getCondition(ifThen)
    val method = ifThen.findAncestorClass(ByteCodeMethodInfo.MethodInfoKey)
    val endLabelName = LabelledLocations.getUniqueLabel("end", method, compilation)
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

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val statementGrammar = grammars.find(StatementSkeleton.StatementGrammar)
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val bodyGrammar = grammars.find(BlockC.BlockOrStatementGrammar)
    val ifThenGrammar = grammars.create(IfThenKey, ("if" ~> ("(" ~> expressionGrammar.as(Condition) ~< ")") ~ bodyGrammar.as(Then)).
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
    Map(IfThenC.getNextLabel(getThenStatements(obj).last) -> next) ++
      super.getLabels(obj)
  }
}
