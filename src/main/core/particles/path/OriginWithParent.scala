package core.particles.path

import core.particles.node.MetaObject

trait OriginWithParent extends Path {
  def parent: Path
  def replaceWith(replacement: MetaObject)
}
