package core.particles.node

trait Key extends AnyRef
{
  override def toString = Node.classDebugRepresentation(this)
}
