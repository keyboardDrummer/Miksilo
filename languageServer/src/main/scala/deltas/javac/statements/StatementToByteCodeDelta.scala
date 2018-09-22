package deltas.javac.statements

import core.deltas._
import core.deltas.path.NodePath
import core.language.node.Node
import core.language.{Compilation, Language}

trait StatementToByteCodeDelta extends Delta with HasShape {

  override def inject(language: Language): Unit = {
    StatementToByteCodeSkeleton.instances.add(language, this)
    super.inject(language)
  }

  def toByteCode(statement: NodePath, compilation: Compilation): Seq[Node]

  case class SequenceDoesNotEndInJump(sequence: Seq[Node]) extends Exception
  {
    override def toString = s"SequenceDoesNotEndInJump: $sequence"
  }
}