package core.particles.path

import core.particles.MetaObject

trait OriginWithParent extends Path {
  def parent: Path
  def replaceWith(replacement: MetaObject)
}
