package core.parsers

import _root_.core.TestSourceUtils
import org.scalatest.FunSuite
import _root_.core.parsers.editorParsers.UntilBestAndXStepsStopFunction

class PerformanceTest extends FunSuite {

  test("Correct JSON performance") {
    import ParseJson._

    val source = TestSourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.json")

    val maxAttempts = 10
    val manySourcesCount = 10
    val manySources = s"[${1.to(manySourcesCount).map(_ => source).reduce((a,b) => a + "," + b)}]"

    var manyRepetitionsTime = Long.MaxValue
    var manySourcesTime = Long.MaxValue

    def average = (manyRepetitionsTime + manySourcesTime) / (2.0 * manySourcesCount)

    var success = false
    var repetition = 0
    while(!success && repetition < maxAttempts) {
      val timeA = System.currentTimeMillis()
      for (_ <- 1.to(manySourcesCount)) {
        val result = jsonParser.getWholeInputParser.parse(new StringReader(source))
        assert(result.successful)
      }

      val timeB = System.currentTimeMillis()
      val result = jsonParser.getWholeInputParser.parse(new StringReader(manySources))
      assert(result.successful)
      val timeC = System.currentTimeMillis()

      manyRepetitionsTime = Math.min(manyRepetitionsTime, timeB - timeA)
      manySourcesTime = Math.min(manySourcesTime, timeC - timeB)
      if (manySourcesTime < manyRepetitionsTime * 1.25 // Verify that performance does not degrade with input length
        && average < 450) {
        success = true
      }
      repetition += 1
    }
    System.out.println(s"manyRepetitions:$manyRepetitionsTime")
    System.out.println(s"manySources:$manySourcesTime")
    System.out.println(s"average:${average}")
    assert(success)
  }

  test("JSON with small errors performance") {
    import ParseJson._

    val program = TestSourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications_edited.json")

    val maxAttempts = 300
    var success = false
    var repetition = 0
    var elapsedTime = Long.MaxValue
    while(!success && repetition < maxAttempts) {
      val timeA = System.currentTimeMillis()
      val result = jsonParser.getWholeInputParser.parse(new StringReader(program), UntilBestAndXStepsStopFunction())
      val timeB = System.currentTimeMillis()
      assert(result.errors.size == 2)
      assert(result.resultOption.head.asInstanceOf[List[_]].size == 6)

      elapsedTime = Math.min(elapsedTime, timeB - timeA)
      if (elapsedTime < 500) {
        success = true
      }
      repetition += 1
    }
    System.out.println(s"time: $elapsedTime")
    assert(elapsedTime < 500)
  }
}



