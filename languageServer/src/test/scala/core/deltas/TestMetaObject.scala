package core.deltas

import core.language.node.{Node, NodeField, NodeShape}
import deltas.expression.IntLiteralDelta
import deltas.expressions.VariableDelta
import org.scalatest.FunSuite
import deltas.javac.methods.call.CallDelta
import deltas.javac.methods.{MemberSelectorDelta, VariableToByteCodeDelta}

class TestMetaObject extends FunSuite {

  test("Equals") {
    val first = new Node(ShapeKey, FieldKey -> FieldValue)
    val second = new Node(ShapeKey, FieldKey -> FieldValue)
    assertResult(first)(second)
  }

  test("EqualsOnJavaModel") {
    val first = CallDelta.neww(MemberSelectorDelta.selector(MemberSelectorDelta.selector(VariableDelta.neww("System"), "out"), "print"),
      List(CallDelta.neww(VariableDelta.neww("fibonacci"), List(IntLiteralDelta.neww(5)))))
    val second = CallDelta.neww(MemberSelectorDelta.selector(MemberSelectorDelta.selector(VariableDelta.neww("System"), "out"), "print"),
      List(CallDelta.neww(VariableDelta.neww("fibonacci"), List(IntLiteralDelta.neww(5)))))
    assertResult(first)(second)
  }

  object ShapeKey extends NodeShape

  object FieldKey extends NodeField

  object FieldValue extends NodeField
}
