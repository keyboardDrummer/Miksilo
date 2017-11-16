package core.deltas.path

import core.deltas.node.Node

trait OriginWithParent extends Path {
  def parent: Path
  def replaceWith(replacement: Any): Unit
}
