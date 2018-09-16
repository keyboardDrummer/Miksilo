package deltas.javac.statements

import core.deltas._
import core.deltas.path.NodePath
import core.language.{Compilation, Language}
import core.language.node.{Node, NodeLike, NodeWrapper}

object ByteCodeStatementSkeleton {

  implicit class Statement[T <: NodeLike](val node: T) extends NodeWrapper[T] { }

  def getToInstructions(compilation: Compilation): NodePath => Seq[Node] = {
    statement => getInstance(compilation, statement).toByteCode(statement, compilation)
  }

  def getInstance(language: Language, statement: NodePath): ByteCodeStatementInstance = {
    instances.get(language, statement.shape)
  }

  val instances = new ShapeProperty[ByteCodeStatementInstance]
}
