package core.deltas

import core.language.Language
import org.scalatest.FunSuite

class DeltaTest extends FunSuite {

  test("missing dependencies are added") {
    object A extends Delta {

      override def description: String = "A"

      override def dependencies: Set[Contract] = Set(B)

      override def inject(language: Language): Unit = {}
    }
    object B extends Delta {

      override def description: String = "B"

      override def dependencies: Set[Contract] = Set.empty
      override def inject(language: Language): Unit = {}
    }

    assertResult(Seq(B, A))(LanguageFromDeltas(Seq(A)).allDeltas)
  }

  test("cycles in the dependency graph are detected") {
    object A extends Delta {

      override def description: String = "A"

      override def dependencies: Set[Contract] = Set(B)
      override def inject(language: Language): Unit = {}
    }
    object B extends Delta {

      override def description: String = "B"

      override def dependencies: Set[Contract] = Set(A)
      override def inject(language: Language): Unit = {}
    }

    assertThrows[DeltaDependencyViolation](LanguageFromDeltas(Seq(A,B)))
  }
}
