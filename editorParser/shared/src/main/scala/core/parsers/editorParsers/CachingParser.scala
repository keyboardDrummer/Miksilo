package core.parsers.editorParsers

import core.parsers.core.{Metrics, NoMetrics, ParseInput}

trait CachingParser[+Result, Input <: ParseInput] {
  def changeRange(start: Int, end: Int, insertionLength: Int): Unit

  def parse(mayStop: StopFunction = StopImmediately,
            metrics: Metrics = NoMetrics): SingleParseResult[Result, Input]
}
