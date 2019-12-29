package core.parsers.editorParsers

import ParseResults._

case class RecursionsList[Input, SeedResult, +Result](recursions: List[RecursiveParseResult[Input, SeedResult, Result]],
                                                      rest: ParseResults[Input, Result])

trait LazyParseResult[Input, +Result] {
  def flatMapReady[NewResult](f: ReadyParseResult[Input, Result] => ParseResults[Input, NewResult],
                              uniform: Boolean): ParseResults[Input, NewResult]

  def mapReady[NewResult](f: ReadyParseResult[Input, Result] => ReadyParseResult[Input, NewResult], uniform: Boolean): LazyParseResult[Input, NewResult]

  val score: Double = (if (history.flawed) 0 else 10000) + history.score

  def history: History[Input]
  def map[NewResult](f: Result => NewResult): LazyParseResult[Input, NewResult]

  def mapWithHistory[NewResult](f: ReadyParseResult[Input, Result] => ReadyParseResult[Input, NewResult],
                                oldHistory: History[Input]): LazyParseResult[Input, NewResult]
}

class DelayedParseResult[Input, Result](val history: History[Input], _getResults: () => ParseResults[Input, Result])
  extends LazyParseResult[Input, Result] {

  override def toString = s"$score delayed: $history"

  override def map[NewResult](f: Result => NewResult): DelayedParseResult[Input, NewResult] = {
    new DelayedParseResult(history, () => results.map(f))
  }

  lazy val results: ParseResults[Input, Result] = _getResults()

  override def mapWithHistory[NewResult](f: ReadyParseResult[Input, Result] => ReadyParseResult[Input, NewResult], oldHistory: History[Input]) =
    new DelayedParseResult(this.history ++ oldHistory, () => {
      val intermediate = this.results
      intermediate.mapWithHistory(f, oldHistory)
    })

  override def mapReady[NewResult](f: ReadyParseResult[Input, Result] => ReadyParseResult[Input, NewResult], uniform: Boolean): DelayedParseResult[Input, NewResult] =
    new DelayedParseResult(this.history, () => {
      val intermediate = this.results
      intermediate.mapReady(f, uniform)
    })

  override def flatMapReady[NewResult](f: ReadyParseResult[Input, Result] => ParseResults[Input, NewResult], uniform: Boolean) =
    singleResult(new DelayedParseResult(this.history, () => {
      val intermediate = this.results
      intermediate.flatMapReady(f, uniform)
    }))
}

case class RecursiveParseResult[Input, SeedResult, +Result](get: ParseResults[Input, SeedResult] => ParseResults[Input, Result]) {

  def compose[NewResult](f: ParseResults[Input, Result] => ParseResults[Input, NewResult]):
    RecursiveParseResult[Input, SeedResult, NewResult] = {

    RecursiveParseResult[Input, SeedResult, NewResult](r => f(get(r)))
  }
}

case class ReadyParseResult[Input, +Result](resultOption: Option[Result], remainder: Input, history: History[Input])
  extends LazyParseResult[Input, Result] {

  val originalScore = (if (history.flawed) 0 else 10000) + history.score
  override val score = 10000 + originalScore

  override def map[NewResult](f: Result => NewResult): ReadyParseResult[Input, NewResult] = {
    ReadyParseResult(resultOption.map(f), remainder, history)
  }

  override def mapWithHistory[NewResult](f: ReadyParseResult[Input, Result] => ReadyParseResult[Input, NewResult], oldHistory: History[Input]) = {
    val newReady = f(this)
    ReadyParseResult(newReady.resultOption, newReady.remainder, newReady.history ++ oldHistory)
  }

  override def mapReady[NewResult](f: ReadyParseResult[Input, Result] => ReadyParseResult[Input, NewResult], uniform: Boolean):
    ReadyParseResult[Input, NewResult] = f(this)

  override def flatMapReady[NewResult](f: ReadyParseResult[Input, Result] => ParseResults[Input, NewResult], uniform: Boolean) = f(this)
}