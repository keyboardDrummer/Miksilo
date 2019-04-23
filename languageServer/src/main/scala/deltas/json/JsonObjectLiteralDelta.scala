package deltas.json

import core.bigrammar.grammars.{Keyword, Parse, RegexGrammar}
import core.deltas.{Delta, DeltaWithGrammar}
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.exceptions.BadInputException
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.expression.{ExpressionDelta, ExpressionInstance}

case class DuplicateObjectLiteralKeys(duplicates: Seq[String]) extends BadInputException

object JsonObjectLiteralDelta extends DeltaWithGrammar with ExpressionInstance with Delta {

  override def description: String = "Adds the JSON object literal to expressions"

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar)
    val keyGrammar = create(MemberKey, RegexGrammar(StringLiteralDelta.stringInnerRegex).
      map[String, String](r => r.substring(1, r.length), s => "\"" + s).as(MemberKey) ~< "\"")
    val member = (keyGrammar ~< ":") ~~ expressionGrammar.as(MemberValue) asNode MemberShape
    val inner = "{" %> (member.manySeparatedVertical(",").as(Members) ~< Parse(Keyword(",") | value(Unit))).indent() %< "}"
    val grammar = inner.asLabelledNode(Shape)
    expressionGrammar.addAlternative(grammar)
  }

  object MemberShape extends NodeShape
  object MemberValue extends NodeField
  object MemberKey extends NodeField

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
    def get(key: String): Option[T] = members.find(member => member.key == key).map(x => x.value)
    def members: Seq[ObjectLiteralMember[T]] = NodeWrapper.wrapList(node(Members).asInstanceOf[Seq[T]])
  }
}
