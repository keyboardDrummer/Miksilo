package deltas.json

import core.bigrammar.grammars.Colorize
import core.bigrammar.{BiGrammar, BiGrammarWriter}
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.node.{Node, NodeField}
import core.language.{Compilation, Language}
import core.parsers.strings.{Position, SourceRange}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{PrimitiveType, Type}
import deltas.expression.{ExpressionDelta, ExpressionInstance, StringLiteralDelta}

import scala.util.matching.Regex

object JsonStringLiteralDelta extends DeltaWithGrammar with ExpressionInstance {

  override def description: String = "Adds the double quoted string literal"

  import StringLiteralDelta.Shape
  val shape = Shape

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  val stringInnerRegex: Regex = """"([^"\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""".r

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val inner = {
      import core.bigrammar.DefaultBiGrammarWriter._
      dropPrefix(grammars, grammars.regexGrammar(stringInnerRegex, "string literal"), Value, "\"") ~<
        BiGrammarWriter.stringToGrammar("\"")
    }
    import grammars._
    val grammar = Colorize(inner, "string.quoted.double")
    find(ExpressionDelta.FirstPrecedenceGrammar).addAlternative(grammar.asLabelledNode(Shape))
  }

  def dropPrefix(grammars: LanguageGrammars, regex: BiGrammar, field: NodeField, prefix: String) = {
    import grammars._
    regex.map[String, String](r => r.substring(prefix.length), s => { prefix + s }).
      as(field, p => SourceRange(Position(p.start.line, p.start.character + prefix.length), p.end))
  }

  def neww(value: String) = new Node(Shape, Value -> value)

  def getValue(literal: Node): String = literal(Value).asInstanceOf[String]

  object Value extends NodeField

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    builder.typesAreEqual(_type, PrimitiveType("String"))
  }
}


