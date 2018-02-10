package deltas.javac.statements

import core.deltas._
import core.deltas.node.{Node, NodeShape}
import core.deltas.path.{ChildPath, Path, SequenceElement}
import core.language.Language
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope

trait StatementInstance extends DeltaWithGrammar {

  override def inject(state: Language): Unit = {
    StatementSkeleton.getRegistry(state).instances.put(key, this)
    super.inject(state)
  }

  def key: NodeShape

  def toByteCode(statement: Path, compilation: Compilation): Seq[Node]

  override def dependencies: Set[Contract] = Set(StatementSkeleton)

  case class SequenceDoesNotEndInJump(sequence: Seq[Node]) extends Exception
  {
    override def toString = s"SequenceDoesNotEndInJump: $sequence"
  }

  def getNextLabel(statement: Path) = (statement, "next") //TODO volgens mij kan dit weg.
  def getNextStatements(obj: Path, labels: Map[Any, Path]): Set[Path] = {
    val selection = obj.asInstanceOf[SequenceElement]
    if (selection.hasNext)
      return Set(selection.next)

    val nextOption = labels.get(getNextLabel(obj))
    if (nextOption.nonEmpty)
      return Set(nextOption.get)

    throw SequenceDoesNotEndInJump(selection.parent.current(selection.field).asInstanceOf[Seq[Node]])
  }

  def getLabels(obj: Path): Map[Any, Path] = Map.empty

  def definedVariables(compilation: Compilation, obj: Node): Map[String, Node] = Map.empty

  def constraints(compilation: Compilation, builder: ConstraintBuilder, statement: ChildPath, parentScope: Scope): Unit
}