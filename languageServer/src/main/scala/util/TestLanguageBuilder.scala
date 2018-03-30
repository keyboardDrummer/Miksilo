package util

import core.deltas._

object TestLanguageBuilder {
  val statistics = new Statistics()

  var compilers : Map[Seq[Delta], TestingLanguage] = Map.empty
  def build(deltas: Seq[Delta], description: String = "testing"): TestingLanguage = {
    val result = compilers.getOrElse(deltas, new TestingLanguage(deltas, description))
    compilers += (deltas -> result)
    result
  }

  def profile[T](description: String, action: => T): T = statistics.profile(description, action)
}