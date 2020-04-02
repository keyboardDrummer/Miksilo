package miksilo.modularLanguages.core.node

trait TypedShape extends NodeShape {

  type Typed[T <: NodeLike] <: NodeWrapper[T]

  def wrap[T <: NodeLike](value: T): Typed[T]

  def from[T <: NodeLike](value: T): Option[Typed[T]] = {
    if (value.shape == this)
      Some(wrap(value))
    else
      None
  }

  def visit(root: NodeLike)(afterChildren: Typed[root.Self] => Unit = child => {},
                            beforeChildren: Typed[root.Self] => Boolean = child => true): Unit = {
    root.visit(afterChild => {
      if (afterChild.shape == this) {
        afterChildren(wrap(afterChild))
      }
    }, beforeChild => {
      if (beforeChild.shape == this)
        beforeChildren(wrap[root.Self](beforeChild))
      else
        true
    })
  }
}

/**
  * Defines a new Node class
  */
trait NodeShape extends GrammarKey {
  def create(values: (NodeField, Any)*): Node = {
    new Node(this, values: _*)
  }

  def createWithData(values: (NodeField, Any)*): Node = {
    val result = new Node(this)
    for(value <- values) {
      value._2 match {
        case fieldData: FieldData => result.setWithData(value._1, fieldData)
        case _ => result(value._1) = value._2
      }
    }
    result
  }
}
