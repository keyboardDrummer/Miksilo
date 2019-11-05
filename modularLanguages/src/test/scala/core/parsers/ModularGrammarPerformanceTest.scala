package core.parsers

import deltas.json.JsonLanguage
import org.scalatest.FunSuite
import util.{SourceUtils, TestLanguageBuilder}
import _root_.core.TestUtils
import _root_.core.parsers.editorParsers.UntilBestAndXStepsStopFunction

class ModularGrammarPerformanceTest extends FunSuite {

  val json = TestLanguageBuilder.buildWithParser(JsonLanguage.deltas, UntilBestAndXStepsStopFunction())

  val modularGrammarSlowdown = 1.3
  val manyRepetitionsTargetTime = PerformanceTest.manyRepetitionsTargetTime * modularGrammarSlowdown
  val manySourcesCount = 10
  val manySourcesTargetTime = manyRepetitionsTargetTime * manySourcesCount / 1.01

  test("Correct JSON small file performance") {
    val source = TestUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.json")
    TestUtils.runPerformanceTest(manyRepetitionsTargetTime, 100, () => {
      val result = json.compileString(source)
      assert(result.program.childElements.size == 6)
      assert(result.diagnostics.isEmpty)
    })
  }

  test("Correct JSON large file performance") {
    val source = TestUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.json")
    val tenTimesSource = s"[${1.to(manySourcesCount).map(_ => source).reduce((a,b) => a + "," + b)}]"

    TestUtils.runPerformanceTest(manySourcesTargetTime, 10, () => {
      val result = json.compileString(tenTimesSource)
      assert(result.program.childElements.size == manySourcesCount)
      assert(result.diagnostics.isEmpty)
    })
  }

  val smallEditsTargetTime = PerformanceTest.smallEditsTargetTime * modularGrammarSlowdown // We only allow the small edits to make the parsing 5ms slower
  test("JSON with small errors performance") {
    val source = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications_edited.json")
    TestUtils.runPerformanceTest(smallEditsTargetTime, 300, () => {
      val result = json.compileString(source)
      assert(result.program.childElements.size == 6)
      assert(result.diagnostics.size == 2)
    })
  }
}
