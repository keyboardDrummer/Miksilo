package core.parsers.editorParsers

import core.parsers.core.{Metrics, NoMetrics}

trait CachingParser[+Result] {
  def changeRange(start: Int, end: Int, insertionLength: Int): Unit

  def parse(mayStop: StopFunction = StopImmediately,
            metrics: Metrics = NoMetrics): SingleParseResult[Result]
}
