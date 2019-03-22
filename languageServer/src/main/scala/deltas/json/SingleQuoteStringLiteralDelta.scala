package deltas.json

import core.bigrammar.grammars.RegexGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.node.{Node, NodeField, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{PrimitiveType, Type}
import deltas.expression.{ExpressionDelta, ExpressionInstance}

object SingleQuoteStringLiteralDelta extends DeltaWithGrammar with ExpressionInstance {

  override def description: String = "Adds the usage of JSON single quoted string literals, " +
    "in which the String is considered an identifier, " +
    "and the quotes are not part of the position"

  val shape = Shape

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._

    val grammar = keyword("'") ~> RegexGrammar("""[^']*""".r).as(DoubleQuoteStringLiteralDelta.Value) ~<
      keyword("'") asLabelledNode DoubleQuoteStringLiteralDelta.Shape
    find(ExpressionDelta.FirstPrecedenceGrammar).addAlternative(grammar)
  }

  def literal(value: String) = new Node(Shape, Value -> value)

  def getValue(literal: Node): String = literal(Value).asInstanceOf[String]

  object Shape extends NodeShape

  object Value extends NodeField

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    builder.typesAreEqual(_type, PrimitiveType("String"))
  }
}
