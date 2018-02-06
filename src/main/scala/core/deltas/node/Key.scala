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



trait Aspect {

}

trait GrammarKey extends Key