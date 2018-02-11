package core.deltas

import core.language.node.{Node, NodeShape, NodeField}
import org.scalatest.FunSuite
import deltas.javac.expressions.literals.IntLiteralDelta
import deltas.javac.methods.call.CallDelta
import deltas.javac.methods.{MemberSelector, VariableDelta}

class TestMetaObject extends FunSuite {

  test("Equals") {
    val first = new Node(ShapeKey, FieldKey -> FieldValue)
    val second = new Node(ShapeKey, FieldKey -> FieldValue)
    assertResult(first)(second)
  }

  test("EqualsOnJavaModel") {
    val first = CallDelta.call(MemberSelector.selector(MemberSelector.selector(VariableDelta.variable("System"), "out"), "print"),
      List(CallDelta.call(VariableDelta.variable("fibonacci"), List(IntLiteralDelta.literal(5)))))
    val second = CallDelta.call(MemberSelector.selector(MemberSelector.selector(VariableDelta.variable("System"), "out"), "print"),
      List(CallDelta.call(VariableDelta.variable("fibonacci"), List(IntLiteralDelta.literal(5)))))
    assertResult(first)(second)
  }

  object ShapeKey extends NodeShape

  object FieldKey extends NodeField

  object FieldValue extends NodeField
}
