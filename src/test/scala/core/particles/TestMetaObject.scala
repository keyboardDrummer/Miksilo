package core.particles

import core.particles.node.{Node, NodeClass, NodeField}
import org.scalatest.FunSuite
import deltas.javac.expressions.literals.IntLiteralDelta
import deltas.javac.methods.call.CallC
import deltas.javac.methods.{MemberSelector, VariableC}

class TestMetaObject extends FunSuite {

  test("Equals") {
    val first = new Node(ClazzKey, FieldKey -> FieldValue)
    val second = new Node(ClazzKey, FieldKey -> FieldValue)
    assertResult(first)(second)
  }

  test("EqualsOnJavaModel") {
    val first = CallC.call(MemberSelector.selector(MemberSelector.selector(VariableC.variable("System"), "out"), "print"),
      List(CallC.call(VariableC.variable("fibonacci"), List(IntLiteralDelta.literal(5)))))
    val second = CallC.call(MemberSelector.selector(MemberSelector.selector(VariableC.variable("System"), "out"), "print"),
      List(CallC.call(VariableC.variable("fibonacci"), List(IntLiteralDelta.literal(5)))))
    assertResult(first)(second)
  }

  object ClazzKey extends NodeClass

  object FieldKey extends NodeField

  object FieldValue extends NodeField
}
