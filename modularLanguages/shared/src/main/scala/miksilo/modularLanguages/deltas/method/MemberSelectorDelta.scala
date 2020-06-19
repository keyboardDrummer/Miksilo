package miksilo.modularLanguages.deltas.javac.methods

import miksilo.modularLanguages.core.deltas._
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.core.node._
import miksilo.modularLanguages.deltas.expression.ExpressionDelta

object MemberSelectorDelta extends DeltaWithGrammar {

  override def description: String = "Defines the selector grammar <expression>.<identifier>"

  implicit class MemberSelector[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def member: String = node.getValue(Member).asInstanceOf[String]
    def target: T = node(Target).asInstanceOf[T]
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val expression = find(ExpressionDelta.LastPrecedenceGrammar)
    (expression.as(Target) ~< ".") ~ identifier.as(Member) asLabelledNode Shape
  }

  object Shape extends NodeShape {
    override def toString: String = "MemberSelector"
  }

  object Target  extends NodeField {
    override def toString: String = "Target"
  }

  object Member extends NodeField {
    override def toString: String = "Member"
  }

  def neww(_object: Any, member: Any): Node = neww(_object.asInstanceOf[Node], member.asInstanceOf[String])

  def neww(_object: Node, member: String): Node = {
    new Node(Shape,
      Target -> _object,
      Member -> member)
  }

  override def dependencies: Set[Contract] = Set(ExpressionDelta)
}
