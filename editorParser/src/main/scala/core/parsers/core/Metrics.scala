package core.parsers.core

trait Metrics {
  def measure(name: String, value: Double)
}

object NoMetrics extends Metrics {
  override def measure(name: String, value: Double): Unit = {}
}