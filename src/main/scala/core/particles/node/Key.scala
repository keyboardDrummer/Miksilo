package core.particles.node

trait Key extends AnyRef
{
  override def toString: String = Node.classDebugRepresentation(this)
}

/**
  * Defines a field for a Node
  */
trait NodeField extends Key

/**
  * Defines a new Node class
  */
trait NodeClass extends Key