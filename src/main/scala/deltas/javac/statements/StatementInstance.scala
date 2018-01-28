package deltas.javac.statements

import core.deltas._
import core.deltas.node.{Node, NodeShape}
import core.deltas.path.{NodePath, NodeSequenceElement}
import core.language.Language

trait StatementInstance extends DeltaWithGrammar {

  override def inject(state: Language): Unit = {
    StatementSkeleton.getRegistry(state).instances.put(key, this)
    super.inject(state)
  }

  def key: NodeShape

  def toByteCode(statement: NodePath, compilation: Compilation): Seq[Node]

  override def dependencies: Set[Contract] = Set(StatementSkeleton)

  case class SequenceDoesNotEndInJump(sequence: Seq[Node]) extends Exception
  {
    override def toString = s"SequenceDoesNotEndInJump: $sequence"
  }

  def getNextLabel(statement: NodePath) = (statement, "next") //TODO volgens mij kan dit weg.
  def getNextStatements(obj: NodePath, labels: Map[Any, NodePath]): Set[NodePath] = {
    val selection = obj.asInstanceOf[NodeSequenceElement]
    if (selection.hasNext)
      return Set(selection.next)

    val nextOption = labels.get(getNextLabel(obj))
    if (nextOption.nonEmpty)
      return Set(nextOption.get)

    throw SequenceDoesNotEndInJump(selection.parent.current(selection.field).asInstanceOf[Seq[Node]])
  }

  def getLabels(obj: NodePath): Map[Any, NodePath] = Map.empty

  def definedVariables(compilation: Compilation, obj: Node): Map[String, Node] = Map.empty
}