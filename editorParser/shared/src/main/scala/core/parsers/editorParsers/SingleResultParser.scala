package core.parsers.editorParsers

import core.parsers.core.{Metrics, NoMetrics, ParseInput, TextPointer}

trait SingleResultParser[+Result, Input <: ParseInput] {
  def parse(text: String,
            mayStop: StopFunction = StopImmediately,
            metrics: Metrics = NoMetrics): SingleParseResult[Result, Input]

  def parse(zero: TextPointer,
            mayStop: StopFunction,
            metrics: Metrics): SingleParseResult[Result, Input]
}
