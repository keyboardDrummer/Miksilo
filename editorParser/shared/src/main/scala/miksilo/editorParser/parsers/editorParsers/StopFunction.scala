package miksilo.editorParser.parsers.editorParsers

object StopFunction {
  val default = UntilTimeStopFunction(100)
}

trait StopFunction {
  def reset(): Unit = {}
  def apply(offset: Int, best: Double, second: Double): Boolean
}

object StopImmediately extends StopFunction {
  override def apply(offset: Int, best: Double, second: Double) = true
}

object NeverStop extends StopFunction {
  override def apply(offset: Int, best: Double, second: Double) = {
    false
  }
}

case class XStepsStopFunction(steps: Int = 2) extends StopFunction {
  var counter = 0

  override def apply(offset: Int, best: Double, second: Double) = {
    counter += 1
    counter > steps  }

  override def reset(): Unit = {
    counter = 0
  }
}

case class UntilBestAndXStepsStopFunction(steps: Int = 1) extends StopFunction {
  var counter = 0
  override def apply(offset: Int, best: Double, second: Double) = {
    (best > second) & {
      counter += 1
      counter > steps
    }
  }

  override def reset(): Unit = {
    counter = 0
  }
}

object StopImmediatelyFunction extends StopFunction {
  override def apply(offset: Int, best: Double, second: Double) = true
}

case class TimeRatioStopFunction(minimumCharsPerMillisecond: Long = 5) extends StopFunction {
  var start: Long = 0

  override def apply(offset: Int, best: Double, second: Double) = {
    val passed = System.currentTimeMillis() - start
    val offsetWithBase = offset + 1000
    val charsPerMillisecond = offsetWithBase / (passed + 1.0)
    minimumCharsPerMillisecond > charsPerMillisecond
  }

  override def reset(): Unit = {
    start = System.currentTimeMillis()
  }
}

case class UntilTimeStopFunction(milliseconds: Long) extends StopFunction {
  var start: Long = 0

  override def apply(offset: Int, best: Double, second: Double) = {
    val passed = System.currentTimeMillis() - start
    passed > milliseconds
  }

  override def reset(): Unit = {
    start = System.currentTimeMillis()
  }
}