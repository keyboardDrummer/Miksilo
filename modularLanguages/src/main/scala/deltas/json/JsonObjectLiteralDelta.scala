package deltas.json

import core.bigrammar.grammars.{Colorize, Delimiter, Keyword, Parse, RegexGrammar}
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{Delta, DeltaWithGrammar}
import core.language.exceptions.BadInputException
import core.language.node._
import core.language.{Compilation, Language}
import core.parsers.editorParsers.History
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.expression.{ExpressionDelta, ExpressionInstance}
import deltas.json.JsonStringLiteralDelta.{dropPrefix, stringInnerRegex}

case class DuplicateObjectLiteralKeys(duplicates: Seq[String]) extends BadInputException

object JsonObjectLiteralDelta extends DeltaWithGrammar with ExpressionInstance with Delta {

  override def description: String = "Adds the JSON object literal to expressions"

  def neww(entries: Map[String, Node]): Node = Shape.create(Members -> entries.map(entry =>
    MemberShape.create(MemberKey -> entry._1, MemberValue -> entry._2)))

  override def transformGrammars(_grammars: LanguageGrammars, language: Language): Unit = {
    val grammars = _grammars

    val keyGrammar = {
      import core.bigrammar.DefaultBiGrammarWriter._
      val regexGrammar = RegexGrammar(stringInnerRegex, "object member key", verifyWhenPrinting = false, Some("\""))
      dropPrefix(grammars, regexGrammar, MemberKey, "\"") ~< "\""
    }
    import _grammars._
    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar)

    val member = (Colorize(create(MemberKey, keyGrammar), "string.quoted.double") ~< ":") ~~ expressionGrammar.as(MemberValue) asNode MemberShape
    val optionalTrailingComma = Parse(Keyword(",") | value(Unit))
    val inner = Delimiter("{", History.missingInputPenalty * 2) %> (member.manySeparatedVertical(",").as(Members) ~< optionalTrailingComma).indent() %< "}"

    val grammar = inner.asLabelledNode(Shape)
    expressionGrammar.addAlternative(grammar)
  }

  object MemberShape extends NodeShape
  object MemberKey extends NodeField
  object MemberValue extends NodeField

  object Members extends NodeField
  object Shape extends NodeShape
  override def shape: NodeShape = Shape

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {

  }

  implicit class ObjectLiteralMember[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def key: String = node.getValue(MemberKey).asInstanceOf[String]
    def value: T = node(MemberValue).asInstanceOf[T]
  }

  implicit class ObjectLiteral[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def getValue(key: String): T = get(key).get
    def getObject(key: String): Option[ObjectLiteral[T]] = get(key).
      flatMap(v => if (v.shape == Shape) Some(ObjectLiteral(v)) else None)

    def get(key: String): Option[T] = members.find(member => member.key == key).map(x => x.value)
    def members: Seq[ObjectLiteralMember[T]] = NodeWrapper.wrapList(node(Members).asInstanceOf[Seq[T]])
  }
}
