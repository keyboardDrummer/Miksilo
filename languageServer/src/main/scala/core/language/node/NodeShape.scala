package core.language.node

/**
  * Defines a new Node class
  */
trait NodeShape extends GrammarKey {
  def create(values: (NodeField, Any)*): Node = {
    new Node(this, values: _*)
  }

  def createWithSource(values: (NodeField, Any)*): Node = {
    val result = new Node(this)
    for(value <- values) {
      value._2 match {
        case withSource: WithSource => result.setWithSource(value._1, withSource)
        case _ => result(value._1) = value._2
      }
    }
    result
  }
}
