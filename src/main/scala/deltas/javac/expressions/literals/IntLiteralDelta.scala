package deltas.javac.expressions.literals

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node.{Node, NodeField, NodeShape}
import core.deltas.path.NodePath
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.constants.IntegerInfoConstant
import deltas.bytecode.coreInstructions.integers.{LoadConstantDelta, SmallIntegerConstantDelta}
import deltas.bytecode.types.IntTypeDelta
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}

object IntLiteralDelta extends ExpressionInstance {
  val shape = Shape

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton, SmallIntegerConstantDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val inner = integer
    val parseNumber = inner.as(Value).asLabelledNode(Shape)
    find(ExpressionSkeleton.ExpressionGrammar).addOption(parseNumber)
  }

  def literal(value: Int) = new Node(Shape, Value -> value)

  override def toByteCode(literal: NodePath, compilation: Compilation): Seq[Node] = {
    val value: Int = getValue(literal)
    if (-1 <= value && value <= 5) {
      val node = literal.current.shallowClone
      node.data.remove(Value)
      node.replaceWith(SmallIntegerConstantDelta.integerConstant(value), keepData = true)
      Seq(node) //TODO dit mooier maken. Maak de nieuwe node gewoon en en schuif deze over de oude node.
    }
    else
    {
      Seq(LoadConstantDelta.integerConstant(IntegerInfoConstant.construct(value)))
    }
  }

  def getValue(literal: Node): Int = literal(Value).asInstanceOf[Int]

  override def getType(expression: NodePath, compilation: Compilation): Node = IntTypeDelta.intType

  object Shape extends NodeShape

  object Value extends NodeField

  override def description: String = "Adds the usage of int literals."

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    builder.typesAreEqual(_type, IntTypeDelta.constraintType)
  }
}
