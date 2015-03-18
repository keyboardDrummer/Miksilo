package core.particles.path

import core.particles.node.Node

trait OriginWithParent extends Path {
  def parent: Path
  def replaceWith(replacement: Node)
}
