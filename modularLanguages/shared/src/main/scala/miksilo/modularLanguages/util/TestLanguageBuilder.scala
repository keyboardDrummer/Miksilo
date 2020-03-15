package miksilo.modularLanguagesutil

import miksilo.modularLanguages.core.deltas._
import miksilo.languageServer.core.language.Language
import miksilo.editorParser.parsers.editorParsers.{StopFunction, StopImmediatelyFunction}

object TestLanguageBuilder {
  val statistics = new Statistics()

  implicit def toLanguage(testingLanguage: TestingLanguage): Language = testingLanguage.language

  var compilers : Map[Seq[Delta], TestingLanguage] = Map.empty

  def buildWithParser(deltas: Seq[Delta],
                      stopFunction: StopFunction = StopImmediatelyFunction,
                      description: String = "testing",
                      indentationSensitive: Boolean = false): TestingLanguage = {
    val deltasWithoutParser = deltas.filter(delta => !delta.isInstanceOf[ParseUsingTextualGrammar])
    build(Seq(ParseUsingTextualGrammar(stopFunction, indentationSensitive)) ++ deltasWithoutParser, description)
  }

  def build(deltas: Seq[Delta], description: String = "testing"): TestingLanguage = {
    val result = compilers.getOrElse(deltas, new TestingLanguage(deltas, description))
    compilers += (deltas -> result)
    result
  }

  def profile[T](description: String, action: => T): T = statistics.profile(description, action)
}