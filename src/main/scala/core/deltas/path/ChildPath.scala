package core.deltas.path

trait ChildPath extends Path {
  type Self <: Path
  def parent: NodePath
  def replaceWith(replacement: Any): Unit
}
