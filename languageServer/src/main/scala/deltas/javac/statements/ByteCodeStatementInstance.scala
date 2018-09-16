package deltas.javac.statements

import core.deltas._
import core.deltas.path.{NodePath, SequenceElement}
import core.language.node.Node
import core.language.{Compilation, Language}

trait ByteCodeStatementInstance extends DeltaWithGrammar with HasShape {

  override def inject(language: Language): Unit = {
    ByteCodeStatementSkeleton.instances.add(language, this)
    super.inject(language)
  }

  def toByteCode(statement: NodePath, compilation: Compilation): Seq[Node]

  case class SequenceDoesNotEndInJump(sequence: Seq[Node]) extends Exception
  {
    override def toString = s"SequenceDoesNotEndInJump: $sequence"
  }

  /** If a statement with this label is found, that statement will be one of the 'next' statements if this statement.
    */
  def getNextLabel(statement: NodePath): (NodePath, String) = (statement, "next")
  def getNextStatements(language: Language, obj: NodePath, labels: Map[Any, NodePath]): Set[NodePath] = {
    val selection = obj.asInstanceOf[SequenceElement]
    if (selection.hasNext)
      return Set(selection.next)

    val nextOption = labels.get(getNextLabel(obj))
    if (nextOption.nonEmpty)
      return Set(nextOption.get)

    throw SequenceDoesNotEndInJump(selection.parent.current(selection.field).asInstanceOf[Seq[Node]])
  }

  /**
    * Returns a map of labels to statements. Can be used for jumping to a particular statement based on that label.
    */
  def getLabels(language: Language, obj: NodePath): Map[Any, NodePath] = Map.empty

  def definedVariables(compilation: Compilation, obj: Node): Map[String, Node] = Map.empty
}