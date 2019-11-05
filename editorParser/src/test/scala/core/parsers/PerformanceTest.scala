package core.parsers

import _root_.core.SourceUtils
import _root_.core.TestUtils
import org.scalatest.FunSuite
import _root_.core.parsers.editorParsers.UntilBestAndXStepsStopFunction

object PerformanceTest {
  val manyRepetitionsTargetTime = 200
  val smallEditsTargetTime = manyRepetitionsTargetTime + 5 // We only allow the small edits to make the parsing 5ms slower
  val manySourcesCount = 10
  val manySourcesTargetTime = manyRepetitionsTargetTime * manySourcesCount * 1.50 // Sadly, the larger file is relatively slower at the moment
}

class PerformanceTest extends FunSuite {
  import PerformanceTest._
  import ParseJson._

  val jsonParser2 = jsonParser.getWholeInputParser

  test("Correct JSON small file performance") {

    val source = SourceUtils.getResourceFileContents("AutoScalingMultiAZWithNotifications.json")

    TestUtils.runPerformanceTest(manyRepetitionsTargetTime, 100, () => {
      val result = jsonParser2.parse(new StringReader(source))
      assert(result.successful)
    })
  }

  test("Correct JSON large file performance") {

    val source = SourceUtils.getResourceFileContents("AutoScalingMultiAZWithNotifications.json")
    val manySources = s"[${1.to(manySourcesCount).map(_ => source).reduce((a,b) => a + "," + b)}]"

    TestUtils.runPerformanceTest(manySourcesTargetTime, 10, () => {
      val result = jsonParser2.parse(new StringReader(manySources))
      assert(result.successful)
    })
  }

  test("JSON with small errors performance") {
    val program = SourceUtils.getResourceFileContents("AutoScalingMultiAZWithNotifications_edited.json")
    TestUtils.runPerformanceTest(smallEditsTargetTime, 300, () => {
      val result = jsonParser2.parse(new StringReader(program), UntilBestAndXStepsStopFunction())
      assert(result.errors.size == 2)
      assert(result.resultOption.head.asInstanceOf[List[_]].size == 6)
    })
  }

}



