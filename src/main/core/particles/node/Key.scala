package core.particles.node

trait Key
{
  override def toString = MetaObject.classDebugRepresentation(this)
}
