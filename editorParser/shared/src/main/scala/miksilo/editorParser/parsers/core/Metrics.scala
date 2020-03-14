package miksilo.editorParser.parsers.core

trait Metrics {
  def measure(name: String, value: Double): Unit
}

object NoMetrics extends Metrics {
  override def measure(name: String, value: Double): Unit = {}
}