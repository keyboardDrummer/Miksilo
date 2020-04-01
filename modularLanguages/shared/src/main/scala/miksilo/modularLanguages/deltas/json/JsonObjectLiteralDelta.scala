package miksilo.modularLanguages.deltas.json

import miksilo.modularLanguages.core.bigrammar.grammars.{Colorize, Delimiter, Keyword, Parse, RegexGrammar}
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.deltas.{Delta, DeltaWithGrammar}
import miksilo.languageServer.core.language.exceptions.BadInputException
import miksilo.modularLanguages.core.node._
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.editorParser.parsers.editorParsers.History
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.Type
import miksilo.modularLanguages.deltas.expression.{ExpressionDelta, ExpressionInstance}
import miksilo.modularLanguages.deltas.json.JsonObjectLiteralDelta.ObjectLiteralMember
import miksilo.modularLanguages.deltas.json.JsonStringLiteralDelta.{dropPrefix, stringInnerRegex}

case class DuplicateObjectLiteralKeys(duplicates: Seq[String]) extends BadInputException

object JsonObjectLiteralDelta extends DeltaWithGrammar with ExpressionInstance with Delta {

  override def description: String = "Adds the JSON object literal to expressions"

  def neww(entries: Map[String, Node]): Node = Shape.create(Members -> entries.map(entry =>
    MemberShape.create(MemberKey -> entry._1, MemberValue -> entry._2)))

  override def transformGrammars(_grammars: LanguageGrammars, language: Language): Unit = {
    import _grammars._
    val grammars = _grammars

    val keyGrammar = {
      import miksilo.modularLanguages.core.bigrammar.DefaultBiGrammarWriter._
      val regexGrammar = RegexGrammar(stringInnerRegex, "object member key", verifyWhenPrinting = false, Some("\""))
      dropPrefix(grammars, regexGrammar, MemberKey, "\"") ~< "\""
    }
    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar)

    val member = (Colorize(create(MemberKey, keyGrammar), "string.quoted.double") ~< ":") ~~ expressionGrammar.as(MemberValue) asNode MemberShape
    val optionalTrailingComma = Parse(Keyword(",") | value(()))
    val inner = Delimiter("{", History.missingInputPenalty * 2) %> (member.manySeparatedVertical(",").as(Members) ~< optionalTrailingComma).indent() %< "}"

    val grammar = inner.asLabelledNode(Shape)
    expressionGrammar.addAlternative(grammar)
  }

  object MemberShape extends TypedShape {
    type Typed[T <: NodeLike] = ObjectLiteralMember[T]

    override def neww[T <: NodeLike](value: T) = ObjectLiteralMember(value)

    override def toString = "Member"
  }

  object MemberKey extends NodeField {
    override def toString = "Key"
  }
  object MemberValue extends NodeField {
    override def toString = "Value"
  }

  object Members extends NodeField {
    override def toString = "Member"
  }

  object Shape extends TypedShape {
    type Typed[T <: NodeLike] = ObjectLiteral[T]

    override def neww[T <: NodeLike](value: T): ObjectLiteral[T] = ObjectLiteral(value)

    override def toString = "Object"
  }

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
