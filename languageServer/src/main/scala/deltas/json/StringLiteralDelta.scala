package deltas.json

import core.bigrammar.BiGrammarWriter
import core.bigrammar.grammars.{BiSequence, Colorize, RegexGrammar}
import core.deltas.{Contract, DeltaWithGrammar}
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{GrammarKey, Node, NodeField, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{PrimitiveType, Type}
import deltas.expression.{ExpressionDelta, ExpressionInstance}

import scala.util.matching.Regex

object StringLiteralDelta extends DeltaWithGrammar with ExpressionInstance {

  override def description: String = "Adds the double quoted string literal"

  val shape = Shape

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  val stringInnerRegex: Regex = """([^"\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""".r

    override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
      val inner = {
        import core.bigrammar.DefaultBiGrammarWriter._
        "\"" ~> RegexGrammar(stringInnerRegex).as(Value) ~< BiGrammarWriter.stringToGrammar("\"")
      }
    import grammars._
    val grammar = create(DoubleQuotedGrammar, Colorize(inner, "string.quoted.double"))
    find(ExpressionDelta.FirstPrecedenceGrammar).addAlternative(grammar.asLabelledNode(Shape))
  }

  object DoubleQuotedGrammar extends GrammarKey

  def literal(value: String) = new Node(Shape, Value -> value)

  def getValue(literal: Node): String = literal(Value).asInstanceOf[String]

  object Shape extends NodeShape

  object Value extends NodeField

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    builder.typesAreEqual(_type, PrimitiveType("String"))
  }
}


