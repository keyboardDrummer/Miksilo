package core.particles.node

trait Key extends AnyRef
{
  override def toString: String = Node.classDebugRepresentation(this)

  override def hashCode(): Int = this.getClass.toString.hashCode
}

/**
  * Defines a field for a Node
  */
trait NodeField extends Key

/**
  * Defines a new Node class
  */
trait NodeClass extends Key {
  def create(values: (NodeField, Any)*): Node = {
    new Node(this, values: _*)
  }
}