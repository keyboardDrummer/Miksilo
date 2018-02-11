package core.language.node

/**
  * Defines a new Node class
  */
trait NodeShape extends GrammarKey {
  def create(values: (NodeField, Any)*): Node = {
    new Node(this, values: _*)
  }
}
