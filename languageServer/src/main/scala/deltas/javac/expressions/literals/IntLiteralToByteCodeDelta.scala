package deltas.javac.expressions.literals

import core.deltas.Contract
import core.deltas.path.NodePath
import core.language.node.{Node, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.constants.IntegerInfoConstant
import deltas.bytecode.coreInstructions.integers.{LoadConstantDelta, SmallIntegerConstantDelta}
import deltas.bytecode.types.IntTypeDelta
import deltas.expression.IntLiteralDelta.{Value, getValue}
import deltas.expression.{ExpressionDelta, HasType2, IntLiteralDelta}
import deltas.javac.expressions.ConvertsToByteCodeDelta

object IntLiteralToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def description: String = "Enabled int literals to convert to bytecode"

  override def inject(language: Language): Unit = {
    ExpressionDelta.expressionInstances.update(language, IntLiteralDelta.Shape, original => new HasType2 {

      override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
        original.constraints(compilation, builder, expression, _type, parentScope)
        builder.typesAreEqual(_type, IntTypeDelta.constraintType)
      }
    })
    super.inject(language)
  }

  override def toByteCode(literal: NodePath, compilation: Compilation): Seq[Node] = {
    val value: Int = getValue(literal)
    if (-1 <= value && value <= 5) {
      val node = literal.current.shallowClone
      node.data.remove(Value)
      node.replaceData(SmallIntegerConstantDelta.integerConstant(value), keepData = true)
      Seq(node) //TODO dit mooier maken. Maak de nieuwe node gewoon en en schuif deze over de oude node.
    }
    else
    {
      Seq(LoadConstantDelta.integerConstant(IntegerInfoConstant.construct(value)))
    }
  }

  override def dependencies: Set[Contract] = Set(IntLiteralDelta, SmallIntegerConstantDelta, LoadConstantDelta)

  override def shape: NodeShape = IntLiteralDelta.Shape
}
