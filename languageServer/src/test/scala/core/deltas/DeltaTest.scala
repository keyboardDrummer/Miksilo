package core.deltas

import org.scalatest.FunSuite

class DeltaTest extends FunSuite {

  test("missing dependencies are added") {
    object A extends Delta {

      override def description: String = "A"

      override def dependencies: Set[Contract] = Set(B)
    }
    object B extends Delta {

      override def description: String = "B"

      override def dependencies: Set[Contract] = Set.empty
    }

    assertResult(Seq(B, A))(Delta.validateDependencies(Seq(A)))
  }

  test("cycles in the dependency graph are detected") {
    object A extends Delta {

      override def description: String = "A"

      override def dependencies: Set[Contract] = Set(B)
    }
    object B extends Delta {

      override def description: String = "B"

      override def dependencies: Set[Contract] = Set(A)
    }

    assertThrows[DeltaDependencyViolation](Delta.buildLanguage(Seq(A,B)))
  }
}
