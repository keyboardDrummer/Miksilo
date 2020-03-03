package core.parsers

import _root_.core.SourceUtils
import _root_.core.TestUtils
import org.scalatest.funsuite.AnyFunSuite
import _root_.core.parsers.editorParsers.UntilBestAndXStepsStopFunction

object PerformanceTest {
  val manyRepetitionsTargetTime = 200
  val smallErrorsTargetTime = manyRepetitionsTargetTime + 5 // We only allow the small errors to make the parsing 5ms slower
  val manySourcesCount = 10
  val manySourcesTargetTime = manyRepetitionsTargetTime * manySourcesCount * 1.75 // Sadly, the larger file is relatively slower at the moment
}

class PerformanceTest extends AnyFunSuite {
  import PerformanceTest._
  import ParseJson._

  val jsonFileParser = jsonParser.getWholeInputParser()

  test("Correct JSON small file performance") {

    val source = SourceUtils.getResourceFileContents("AutoScalingMultiAZWithNotifications.json")

    TestUtils.runPerformanceTest(manyRepetitionsTargetTime, 100, () => {
      val result = jsonFileParser.resetAndParse(source)
      assert(result.successful)
    })
  }

  test("Correct JSON large file performance") {

    val source = SourceUtils.getResourceFileContents("AutoScalingMultiAZWithNotifications.json")
    val manySources = s"[${1.to(manySourcesCount).map(_ => source).reduce((a,b) => a + "," + b)}]"

    TestUtils.runPerformanceTest(manySourcesTargetTime, 10, () => {
      val result = jsonFileParser.resetAndParse(manySources)
      assert(result.successful, result.errors)
    })
  }

  test("JSON with small errors performance") {
    val program = SourceUtils.getResourceFileContents("AutoScalingMultiAZWithNotifications_edited.json")
    TestUtils.runPerformanceTest(smallErrorsTargetTime, 300, () => {
      val result = jsonFileParser.resetAndParse(program, UntilBestAndXStepsStopFunction())
      assert(result.errors.size == 2)
      assert(result.resultOption.head.asInstanceOf[List[_]].size == 6)
    })
  }

}



