package core

object TestUtils {

  def runPerformanceTest(targetTime: Double, maxRepetitions: Integer, action: () => Unit): Unit = {
    var repetition = 0
    var success = false
    var bestTime = Double.MaxValue
    while(!success && repetition < maxRepetitions) {
      val before = System.currentTimeMillis()
      action()
      val after = System.currentTimeMillis()
      val newTime = after - before
      bestTime = Math.min(bestTime, newTime)
      if (bestTime <= targetTime) {
        success = true
      }
      repetition += 1
    }
    System.out.println(s"time: $bestTime")
    System.out.println(s"repetitions: $repetition")
    assert(success, s"only reached $bestTime instead of target time $targetTime")
  }
}
