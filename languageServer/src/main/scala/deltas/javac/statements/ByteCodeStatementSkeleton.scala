package deltas.javac.statements

import core.deltas._
import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.{Node, NodeLike, NodeWrapper}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope

object ByteCodeStatementSkeleton {

  implicit class Statement[T <: NodeLike](val node: T) extends NodeWrapper[T] { }

  def getToInstructions(compilation: Compilation): NodePath => Seq[Node] = {
    statement => getInstance(compilation, statement).toByteCode(statement, compilation)
  }

  def getInstance(compilation: Compilation, statement: NodePath): StatementInstance = {
    instances.get(compilation, statement.shape)
  }

  val instances = new ShapeProperty[StatementInstance]

  def constraints(compilation: Compilation, builder: ConstraintBuilder, statement: NodePath, parentScope: Scope): Unit = {
    getInstance(compilation, statement).constraints(compilation, builder, statement, parentScope)
  }
}
