package core.parsers

import org.scalatest.FunSuite
import _root_.core.parsers.editorParsers.TimeRatioStopFunction

class StopFunctionTest extends FunSuite {

  test("TimeRatioStopFunction") {
    val stopFunction = new TimeRatioStopFunction()
    assert(stopFunction.apply(0,0,0))
  }
}
