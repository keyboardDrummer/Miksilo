package core.parsers

import _root_.core.TestSourceUtils
import org.scalatest.FunSuite
import _root_.core.parsers.editorParsers.UntilBestAndXStepsStopFunction

class PerformanceTest extends FunSuite {

  test("Errorless JSON performance") {
    import ParseJson._

    val source = TestSourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.json").
      replaceAll("\\s", "")

    val multiplier = 1
    val tenTimesSource = s"[${1.to(10).map(_ => source).reduce((a,b) => a + "," + b)}]"

    val timeA = System.currentTimeMillis()
    for(_ <- 1.to(multiplier * 10)) {
      val result = jsonParser.getWholeInputParser.parse(new StringReader(source))
      assert(result.successful)
    }

    val timeB = System.currentTimeMillis()
    for(_ <- 1.to(multiplier)) {
      val result = jsonParser.getWholeInputParser.parse(new StringReader(tenTimesSource))
      assert(result.successful)
    }

    val timeC = System.currentTimeMillis()

    val singleSource = timeB - timeA
    val sourceTimesTen = timeC - timeB
    assert(singleSource < 3000 * multiplier)
    System.out.println(s"singleSource:$singleSource")
    System.out.println(s"totalTime:${singleSource + sourceTimesTen}")
  }

  test("Edited") {
    import ParseJson._

    val program = TestSourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications_edited.json")
    val timeA = System.currentTimeMillis()

    val repetitions = 5
    for(_ <- 1.to(repetitions)) {
      val result = jsonParser.getWholeInputParser.parse(new StringReader(program), UntilBestAndXStepsStopFunction())
      assert(result.errors.size == 2)
      assert(result.resultOption.head.asInstanceOf[List[_]].size == 6)
    }
    val timeB = System.currentTimeMillis()
    val elapsedTime = timeB - timeA
    val average = elapsedTime / repetitions
    System.out.println(s"edited average: $average")
    assert(average < 1200)
  }
}



