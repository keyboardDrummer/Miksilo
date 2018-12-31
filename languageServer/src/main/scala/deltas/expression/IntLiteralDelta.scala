package deltas.expression

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{Node, NodeField, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.types.IntTypeDelta
import deltas.expressions.ExpressionDelta
import deltas.javac.expressions.ExpressionInstance

object IntLiteralDelta extends DeltaWithGrammar with ExpressionInstance {
  val shape = Shape

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val inner = integer
    val parseNumber = inner.as(Value).asLabelledNode(Shape)
    find(ExpressionDelta.FirstPrecedenceGrammar).addAlternative(parseNumber)
  }

  def neww(value: Int) = new Node(Shape, Value -> value)

  def getValue(literal: Node): Int = literal(Value).asInstanceOf[Int]

  override def getType(expression: NodePath, compilation: Compilation): Node = IntTypeDelta.intType

  object Shape extends NodeShape

  object Value extends NodeField

  override def description: String = "Adds the usage of int literals."

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    builder.typesAreEqual(_type, IntTypeDelta.constraintType)
  }
}


