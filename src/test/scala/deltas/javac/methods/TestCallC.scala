package deltas.javac.methods

import core.language.Language
import deltas.javac.expressions.ExpressionSkeleton
import deltas.javac.methods.call.{CallDelta, CallStaticOrInstanceDelta}
import org.scalatest.FunSuite

class TestCallC extends FunSuite {

  test("callC") {
    val language = new Language(Seq.empty)
    ExpressionSkeleton.inject(language)
    MemberSelector.inject(language)
    CallStaticOrInstanceDelta.inject(language)
    assert(ExpressionSkeleton.getRegistry(language).instances.get(CallDelta.CallKey).nonEmpty)
  }
}
