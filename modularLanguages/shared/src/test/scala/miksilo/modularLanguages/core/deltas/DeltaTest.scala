package miksilo.modularLanguages.core.deltas

import miksilo.languageServer.core.language.Language
import org.scalatest.funsuite.AnyFunSuite

class DeltaTest extends AnyFunSuite {

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

    assertResult(Seq(B, A))(LanguageFromDeltas(Seq(A), addMissingDeltas = true).allDeltas)
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
