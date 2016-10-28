package core.particles

import core.particles.node.Node
import org.junit.{Assert, Test}
import org.scalatest.FunSuite
import transformations.javac.expressions.literals.IntLiteralC
import transformations.javac.methods.call.CallC
import transformations.javac.methods.{MemberSelector, VariableC}

class TestMetaObject extends FunSuite {


  def testEquals() {
    val first = new Node(ClazzKey, FieldKey -> FieldValue)
    val second = new Node(ClazzKey, FieldKey -> FieldValue)
    assertResult(first)( second)
  }


  def testEqualsOnJavaModel() {
    val first = CallC.call(MemberSelector.selector(MemberSelector.selector(VariableC.variable("System"), "out"), "print"),
      List(CallC.call(VariableC.variable("fibonacci"), List(IntLiteralC.literal(5)))))
    val second = CallC.call(MemberSelector.selector(MemberSelector.selector(VariableC.variable("System"), "out"), "print"),
      List(CallC.call(VariableC.variable("fibonacci"), List(IntLiteralC.literal(5)))))
    assertResult(first)(second)
  }

  object ClazzKey

  object FieldKey

  object FieldValue


}
