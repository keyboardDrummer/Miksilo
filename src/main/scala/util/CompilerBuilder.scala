package util

import java.util.{Timer, TimerTask}

import core.deltas._

object CompilerBuilder {
  val statistics = new Statistics()

  var compilers : Map[Seq[Delta], TestingLanguage] = Map.empty
  def build(deltas: Seq[Delta], description: String = "testing"): TestingLanguage = {
    val result = compilers.getOrElse(deltas, new TestingLanguage(deltas, description))
    compilers += (deltas -> result)
    result
  }

  def profile[T](description: String, action: => T): T = statistics.profile(description, action)

  val timer = new Timer()
  timer.scheduleAtFixedRate(new MyThread(), 5000, 5000)

  class MyThread extends TimerTask {
    override def run(): Unit = {
      System.out.println("\nProfiling global results:")
      statistics.printAll()
    }
  }
}