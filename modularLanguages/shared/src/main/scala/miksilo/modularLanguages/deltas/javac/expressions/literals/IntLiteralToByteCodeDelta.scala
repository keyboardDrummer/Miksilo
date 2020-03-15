package miksilo.modularLanguages.deltas.javac.expressions.literals

import miksilo.modularLanguages.core.deltas.Contract
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node.{Node, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.Type
import miksilo.modularLanguages.deltas.bytecode.constants.IntegerInfoConstant
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.integers.{LoadConstantDelta, SmallIntegerConstantDelta}
import miksilo.modularLanguages.deltas.bytecode.types.IntTypeDelta
import miksilo.modularLanguages.deltas.expression.IntLiteralDelta.{Value, getValue}
import miksilo.modularLanguages.deltas.expression.{ExpressionDelta, IntLiteralDelta, IsExpression}
import miksilo.modularLanguages.deltas.javac.expressions.ConvertsToByteCodeDelta

object IntLiteralToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def description: String = "Enabled int literals to convert to bytecode"

  override def inject(language: Language): Unit = {
    ExpressionDelta.expressionInstances.change(language, IntLiteralDelta.Shape, original => new IsExpression {

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
