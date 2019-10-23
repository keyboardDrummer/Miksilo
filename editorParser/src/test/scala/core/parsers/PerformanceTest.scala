package core.parsers

import _root_.core.TestSourceUtils
import org.scalatest.FunSuite
import _root_.core.parsers.editorParsers.UntilBestAndXStepsStopFunction

class PerformanceTest extends FunSuite {

  test("Correct JSON performance") {
    import ParseJson._

    val source = TestSourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.json")

    val multiplier = 15
    val manySourcesCount = 10
    val manySources = s"[${1.to(manySourcesCount).map(_ => source).reduce((a,b) => a + "," + b)}]"

    var manyRepetitions = 0L
    var manySourcesTime = 0L
    for(_ <- 1.to(multiplier)) {
      val timeA = System.currentTimeMillis()
      for (_ <- 1.to(manySourcesCount)) {
        val result = jsonParser.getWholeInputParser.parse(new StringReader(source))
        assert(result.successful)
      }

      val timeB = System.currentTimeMillis()
      val result = jsonParser.getWholeInputParser.parse(new StringReader(manySources))
      assert(result.successful)
      val timeC = System.currentTimeMillis()

      manyRepetitions += timeB - timeA
      manySourcesTime += timeC - timeB
    }

    val average = (manyRepetitions + manySourcesTime) / (2 * manySourcesCount * multiplier)
    assert(manySourcesTime < manyRepetitions * 1.1) // Verify that performance does not degrade with input length
    assert(average < 150)
    System.out.println(s"manyRepetitions:$manyRepetitions")
    System.out.println(s"manySources:$manySourcesTime")
    System.out.println(s"average:${average}")
  }

  test("JSON with small errors performance") {
    import ParseJson._

    val program = TestSourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications_edited.json")
    val timeA = System.currentTimeMillis()

    val repetitions = 300
    for(_ <- 1.to(repetitions)) {
      val result = jsonParser.getWholeInputParser.parse(new StringReader(program), UntilBestAndXStepsStopFunction())
      assert(result.errors.size == 2)
      assert(result.resultOption.head.asInstanceOf[List[_]].size == 6)
    }
    val timeB = System.currentTimeMillis()
    val elapsedTime = timeB - timeA
    val average = elapsedTime / repetitions
    System.out.println(s"edited average: $average")
    assert(average < 500)
  }
}



