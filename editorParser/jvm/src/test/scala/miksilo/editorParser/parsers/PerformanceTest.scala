package miksilo.editorParser.parsers

import miksilo.editorParser.TestUtils
import miksilo.editorParser.SourceUtils
import miksilo.editorParser.languages.json.{JsonObject, JsonParser}
import miksilo.editorParser.parsers.editorParsers.UntilBestAndXStepsStopFunction
import org.scalatest.funsuite.AnyFunSuite

object PerformanceTest {
  val manyRepetitionsTargetTime = 250
  val smallErrorsTargetTime = manyRepetitionsTargetTime + 5 // We only allow the small errors to make the parsing 5ms slower
  val manySourcesCount = 10
  val manySourcesTargetTime = manyRepetitionsTargetTime * manySourcesCount * 1.50 // Sadly, the larger file is relatively slower at the moment
}

class PerformanceTest extends AnyFunSuite {
  import PerformanceTest._

  val jsonFileParser = JsonParser.parser

  test("Correct JSON small file performance") {

    val source = SourceUtils.getResourceFileContents("AutoScalingMultiAZWithNotifications.json")

    TestUtils.runPerformanceTest(manyRepetitionsTargetTime, 100, () => {
      val result = jsonFileParser.parse(source)
      assert(result.successful)
    })
  }

  test("Correct JSON large file performance") {

    val source = SourceUtils.getResourceFileContents("AutoScalingMultiAZWithNotifications.json")
    val manySources = s"[${1.to(manySourcesCount).map(_ => source).reduce((a,b) => a + "," + b)}]"

    TestUtils.runPerformanceTest(manySourcesTargetTime, 10, () => {
      val result = jsonFileParser.parse(manySources)
      assert(result.successful, result.errors)
    })
  }

  test("JSON with small errors performance") {
    val program = SourceUtils.getResourceFileContents("AutoScalingMultiAZWithNotifications_edited.json")
    TestUtils.runPerformanceTest(smallErrorsTargetTime, 300, () => {
      val result = jsonFileParser.parse(program, UntilBestAndXStepsStopFunction())
      assert(result.errors.size == 2)
      assert(result.resultOption.head.asInstanceOf[JsonObject].members.length == 6)
    })
  }

}



