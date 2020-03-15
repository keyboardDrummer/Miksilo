package miksilo.editorParser.parsers.editorParsers

import miksilo.editorParser.parsers.core.{Metrics, NoMetrics, TextPointer}

trait SingleResultParser[+Result] {
  def parse(text: String,
            mayStop: StopFunction = StopImmediately,
            metrics: Metrics = NoMetrics): SingleParseResult[Result]

  def parse(zero: TextPointer,
            mayStop: StopFunction,
            metrics: Metrics): SingleParseResult[Result]
}
