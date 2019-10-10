package deltas.javac.methods

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node._
import deltas.expression.ExpressionDelta

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

  object Shape extends NodeShape

  object Target  extends NodeField

  object Member extends NodeField

  def neww(_object: Any, member: Any): Node = neww(_object.asInstanceOf[Node], member.asInstanceOf[String])

  def neww(_object: Node, member: String): Node = {
    new Node(Shape,
      Target -> _object,
      Member -> member)
  }

  override def dependencies: Set[Contract] = Set(ExpressionDelta)
}
