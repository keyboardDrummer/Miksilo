package core.particles.node

trait Key
{
  override def toString = Node.classDebugRepresentation(this)
}
