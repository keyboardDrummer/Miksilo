package core.particles.path

import core.particles.{Key, MetaObject}

case class Root(current: MetaObject) extends Path with Key{
  override def parentOption: Option[Path] = None

  override def hashCode(): Int = 1 //obj.hashCode

  override def equals(obj: Any): Boolean = obj.isInstanceOf[Root] //&& obj.equals..
}






