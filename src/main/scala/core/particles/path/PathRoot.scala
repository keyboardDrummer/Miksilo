package core.particles.path

import core.particles.node.{Node, Key}

case class PathRoot(current: Node) extends Path with Key {
  override def parentOption: Option[Path] = None

  override def hashCode(): Int = 1 //obj.hashCode

  override def equals(obj: Any): Boolean = obj.isInstanceOf[PathRoot] //&& obj.equals..
  override def pathAsString: String = "Root"
}






