package deltas.json

import core.bigrammar.BiGrammar
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

  def neww(entries: Map[String, Node]): Node = Shape.create(Members -> entries.map(entry =>
    MemberShape.create(MemberKey -> entry._1, MemberValue -> entry._2)))

  override def transformGrammars(_grammars: LanguageGrammars, language: Language): Unit = {
    val grammars = _grammars
    import _grammars._

    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar)
    val keyGrammar = create(MemberKey, "\"" ~> RegexGrammar(StringLiteralDelta.stringInnerRegex).as(MemberKey) ~< "\"")
    val member = (keyGrammar ~< ":") ~~ expressionGrammar.as(MemberValue) asNode MemberShape
    val inner = "{" %> commaSeparatedVertical(grammars, member).as(Members).indent() %< "}"

    val grammar = inner.asLabelledNode(Shape)
    expressionGrammar.addAlternative(grammar)
  }

  def commaSeparatedVertical(grammars: LanguageGrammars, member: BiGrammar) = {
    import grammars._
    ((member ~< "," ~< printSpace).manyVertical % member.option).map[(Seq[Any], Option[Any]), Seq[Any]](t => {
      t._2.fold(t._1)(last => t._1 ++ Seq(last))
    }, r => if (r.nonEmpty) (r.dropRight(1), Some(r.last)) else (Seq.empty, None))
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
