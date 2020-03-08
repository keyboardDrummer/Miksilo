package core.parsers.editorParsers

import core.parsers.core.{Metrics, NoMetrics, TextPointer}

trait SingleResultParser[+Result] {
  def parse(text: String,
            mayStop: StopFunction = StopImmediately,
            metrics: Metrics = NoMetrics): SingleParseResult[Result]

  def parse(zero: TextPointer,
            mayStop: StopFunction,
            metrics: Metrics): SingleParseResult[Result]
}
