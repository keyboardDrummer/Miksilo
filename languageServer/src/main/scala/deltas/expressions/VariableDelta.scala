package deltas.expressions

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{GrammarKey, Node, NodeField, NodeShape}

object VariableDelta extends DeltaWithGrammar {

  object VariableGrammar extends GrammarKey

  def neww(name: String) = new Node(Shape, Name -> name)

  object Name extends NodeField

  object Shape extends NodeShape

  def getVariableName(variable: Node): String = variable(Name).asInstanceOf[String]

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val core = find(ExpressionDelta.LastPrecedenceGrammar)
    val variableGrammar = create(VariableGrammar, identifier.as(Name) asNode Shape)
    core.addAlternative(variableGrammar)
  }

  override def description: String = "Enables referencing a variable."
}
