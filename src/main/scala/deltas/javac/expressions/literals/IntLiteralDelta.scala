package deltas.javac.expressions.literals

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeField, NodeShape}
import core.deltas.path.Path
import core.language.Language
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.constants.IntegerInfoConstant
import deltas.bytecode.coreInstructions.integers.{LoadConstantDelta, SmallIntegerConstantDelta}
import deltas.bytecode.types.IntTypeDelta
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}

object IntLiteralDelta extends ExpressionInstance {
  val key = Shape

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton, SmallIntegerConstantDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val inner = integer
    val parseNumber = inner.as(Value).asLabelledNode(Shape)
    find(ExpressionSkeleton.ExpressionGrammar).addOption(parseNumber)
  }

  def literal(value: Int) = new Node(Shape, Value -> value)

  override def toByteCode(literal: Path, compilation: Compilation): Seq[Node] = {
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

  override def getType(expression: Path, compilation: Compilation): Node = IntTypeDelta.intType

  object Shape extends NodeShape

  object Value extends NodeField

  override def description: String = "Adds the usage of int literals."

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: Path, _type: Type, parentScope: Scope): Unit = {
    builder.typesAreEqual(_type, IntTypeDelta.constraintType)
  }
}
