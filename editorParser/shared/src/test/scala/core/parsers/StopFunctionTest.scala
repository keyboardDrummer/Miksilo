package core.parsers

import org.scalatest.funsuite.AnyFunSuite
import _root_.core.parsers.editorParsers.TimeRatioStopFunction

class StopFunctionTest extends AnyFunSuite {

  test("TimeRatioStopFunction") {
    val stopFunction = new TimeRatioStopFunction()
    assert(stopFunction.apply(0,0,0))
  }
}
