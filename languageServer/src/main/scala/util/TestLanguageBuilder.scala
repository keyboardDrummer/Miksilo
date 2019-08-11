package util

import core.deltas._
import core.parsers.editorParsers.StopFunction
import core.parsers.sequences.UntilBestAndXStepsStopFunction

object TestLanguageBuilder {
  val statistics = new Statistics()

  var compilers : Map[Seq[Delta], TestingLanguage] = Map.empty

  def buildWithParser(deltas: Seq[Delta],
                      stopFunction: StopFunction = UntilBestAndXStepsStopFunction(),
                      description: String = "testing"): TestingLanguage = {
    val deltasWithoutParser = deltas.filter(delta => !delta.isInstanceOf[ParseUsingTextualGrammar])
    build(Seq(ParseUsingTextualGrammar(stopFunction)) ++ deltasWithoutParser, description)
  }

  def build(deltas: Seq[Delta], description: String = "testing"): TestingLanguage = {
    val result = compilers.getOrElse(deltas, new TestingLanguage(deltas, description))
    compilers += (deltas -> result)
    result
  }

  def profile[T](description: String, action: => T): T = statistics.profile(description, action)
}