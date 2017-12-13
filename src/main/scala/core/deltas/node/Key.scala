package core.deltas.node

trait Key extends AnyRef
{
  override lazy val toString: String = Node.shapeDebugRepresentation(this)

  /**
    * This hashcode does not change over runs, while the default hashcode does.
    * This makes the compilation process more deterministic.
    */
  override def hashCode(): Int = this.getClass.toString.hashCode
}

/**
  * Defines a field for a Node
  */
trait NodeField extends GrammarKey

/**
  * Defines a new Node class
  */
trait NodeShape extends GrammarKey {
  def create(values: (NodeField, Any)*): Node = {
    new Node(this, values: _*)
  }
}

trait GrammarKey extends Key