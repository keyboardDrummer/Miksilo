package deltas.json

import core.bigrammar.grammars.RegexGrammar
import core.deltas.{Contract, DeltaWithGrammar}
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{Node, NodeField, NodeShape, SourceRange}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{PrimitiveType, Type}
import deltas.expression.{ExpressionDelta, ExpressionInstance}
import langserver.types.Position

import scala.util.matching.Regex

object StringLiteralDelta extends DeltaWithGrammar with ExpressionInstance {

  override def description: String = "Adds the double quoted string literal"

  val shape = Shape

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  val stringInnerRegex: Regex = """"([^"\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""".r

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val r = RegexGrammar(stringInnerRegex)
    val innerGrammar = r.
      map[String, String](r => r.substring(1, r.length), s => "\"" + s).as(Value,
      p => SourceRange(Position(p.start.line, p.start.character + 1), p.end)) ~< "\""
    val grammar = innerGrammar.asLabelledNode(Shape)
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


