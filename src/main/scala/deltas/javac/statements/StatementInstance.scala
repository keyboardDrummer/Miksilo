package deltas.javac.statements

import core.deltas._
import core.language.node.{Node, NodeShape}
import core.deltas.path.{ChildPath, NodePath, SequenceElement}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope

trait StatementInstance extends DeltaWithGrammar with HasShape {

  override def inject(language: Language): Unit = {
    StatementSkeleton.instances.add(language, this)
    super.inject(language)
  }

  def shape: NodeShape

  def toByteCode(statement: NodePath, compilation: Compilation): Seq[Node]

  override def dependencies: Set[Contract] = Set(StatementSkeleton)

  case class SequenceDoesNotEndInJump(sequence: Seq[Node]) extends Exception
  {
    override def toString = s"SequenceDoesNotEndInJump: $sequence"
  }

  def getNextLabel(statement: NodePath) = (statement, "next") //TODO volgens mij kan dit weg.
  def getNextStatements(obj: NodePath, labels: Map[Any, NodePath]): Set[NodePath] = {
    val selection = obj.asInstanceOf[SequenceElement]
    if (selection.hasNext)
      return Set(selection.next)

    val nextOption = labels.get(getNextLabel(obj))
    if (nextOption.nonEmpty)
      return Set(nextOption.get)

    throw SequenceDoesNotEndInJump(selection.parent.current(selection.field).asInstanceOf[Seq[Node]])
  }

  def getLabels(obj: NodePath): Map[Any, NodePath] = Map.empty

  def definedVariables(compilation: Compilation, obj: Node): Map[String, Node] = Map.empty

  def constraints(compilation: Compilation, builder: ConstraintBuilder, statement: ChildPath, parentScope: Scope): Unit
}