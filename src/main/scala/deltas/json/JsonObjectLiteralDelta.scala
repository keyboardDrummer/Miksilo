package deltas.json

import core.bigrammar.grammars.{Keyword, Parse, StringLiteral}
import core.deltas.DeltaWithPhase
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.exceptions.BadInputException
import core.language.node.{Node, NodeField, NodeShape, NodeWrapper}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}

case class DuplicateObjectLiteralKeys(duplicates: Seq[String]) extends BadInputException

object JsonObjectLiteralDelta extends ExpressionInstance with DeltaWithPhase {

  override def description: String = "Adds the JSON object literal to expressions"

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val expressionGrammar = find(ExpressionSkeleton.ExpressionGrammar)
    val member = StringLiteral.as(MemberKey) ~ ":" ~~ expressionGrammar.as(MemberValue) asNode MemberShape
    val inner = "{" % (member.manySeparatedVertical(",").as(Members) ~ Parse(Keyword(",") | value(Unit))).indent() % "}"
    val grammar = inner.asLabelledNode(Shape)
    expressionGrammar.addAlternative(grammar)
  }

  object MemberValue extends NodeField
  object MemberKey extends NodeField
  object MemberShape extends NodeShape

  object Members extends NodeField
  object Shape extends NodeShape
  override def shape: NodeShape = Shape

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {

  }

  override def getType(expression: NodePath, compilation: Compilation): Node = ???

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
//    program.visitShape(Shape, objectLiteral => {
//      val membersList = objectLiteral(Member).asInstanceOf[Seq[Node]]
//      val keys = membersList.map(member => member(MemberKey).asInstanceOf[String])
//      val duplicateKeys: Seq[String] = keys.groupBy(identity).collect { case (x, List(_,_,_*)) => x }.toSeq
//      if (duplicateKeys.nonEmpty)
//        throw DuplicateObjectLiteralKeys(duplicateKeys)
//
//      objectLiteral(Members) = membersList.map(member => (member(MemberKey).asInstanceOf[String], member(MemberValue))).toMap
//    })
  }

  implicit class ObjectLiteralMember(val node: Node) extends NodeWrapper[Node] {
    def key: String = node(MemberKey).asInstanceOf[String]
    def keyWithQuotes: String = "\"" + key + "\""
    def value: Node = node(MemberValue).asInstanceOf[Node]
  }

  implicit class ObjectLiteral(val node: Node) extends NodeWrapper[Node] {
    def getValue(key: String): Node = members.find(member => member.key == key).get.value
    def members: Seq[ObjectLiteralMember] = NodeWrapper.wrapList(node(Members).asInstanceOf[Seq[Node]])
  }

//  implicit class ObjectLiteral(val node: Node) extends NodeWrapper[Node] {
//    def entries: Seq[(String, Node)] = node(Members).asInstanceOf[Map[String,Node]].toSeq
//    def getValue(key: String): Any = node(Members).asInstanceOf[Map[String,Node]](key)
//  }
}
