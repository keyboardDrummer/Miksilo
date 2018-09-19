package deltas.javac.statements

import core.deltas._
import core.deltas.path.NodePath
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

  def definedVariables(compilation: Compilation, obj: Node): Map[String, Node] = Map.empty
}