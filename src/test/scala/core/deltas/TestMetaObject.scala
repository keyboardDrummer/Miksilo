package core.deltas

import core.deltas.node.{Node, NodeShape, NodeField}
import org.scalatest.FunSuite
import deltas.javac.expressions.literals.IntLiteralDelta
import deltas.javac.methods.call.CallC
import deltas.javac.methods.{MemberSelector, VariableDelta}

class TestMetaObject extends FunSuite {

  test("Equals") {
    val first = new Node(ShapeKey, FieldKey -> FieldValue)
    val second = new Node(ShapeKey, FieldKey -> FieldValue)
    assertResult(first)(second)
  }

  test("EqualsOnJavaModel") {
    val first = CallC.call(MemberSelector.selector(MemberSelector.selector(VariableDelta.variable("System"), "out"), "print"),
      List(CallC.call(VariableDelta.variable("fibonacci"), List(IntLiteralDelta.literal(5)))))
    val second = CallC.call(MemberSelector.selector(MemberSelector.selector(VariableDelta.variable("System"), "out"), "print"),
      List(CallC.call(VariableDelta.variable("fibonacci"), List(IntLiteralDelta.literal(5)))))
    assertResult(first)(second)
  }

  object ShapeKey extends NodeShape

  object FieldKey extends NodeField

  object FieldValue extends NodeField
}
