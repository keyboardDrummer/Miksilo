package core.parsers.editorParsers

import ParseResults._
import core.parsers.core.{InputGen, TextPointer}

case class RecursionsList[State, SeedResult, +Result](
  recursions: List[RecursiveParseResult[State, SeedResult, Result]],
  rest: ParseResults[State, Result])

trait LazyParseResult[State, +Result] {
  def offset: TextPointer

  def flatMapReady[NewResult](f: ReadyParseResult[State, Result] => ParseResults[State, NewResult],
                              uniform: Boolean): ParseResults[State, NewResult]

  def mapReady[NewResult](f: ReadyParseResult[State, Result] => ReadyParseResult[State, NewResult], uniform: Boolean): LazyParseResult[State, NewResult]

  val score: Double = (if (history.flawed) 0 else 10000) + history.score

  def history: History
  def map[NewResult](f: Result => NewResult): LazyParseResult[State, NewResult]

  def mapWithHistory[NewResult](f: ReadyParseResult[State, Result] => ReadyParseResult[State, NewResult],
                                oldHistory: History): LazyParseResult[State, NewResult]
}

class DelayedParseResult[State, Result](val remainder: InputGen[State], val history: History, _getResults: () => ParseResults[State, Result])
  extends LazyParseResult[State, Result] {

  override def toString = s"$score delayed: $history"

  override def map[NewResult](f: Result => NewResult): DelayedParseResult[State, NewResult] = {
    new DelayedParseResult(remainder, history, () => results.map(f))
  }

  lazy val results: ParseResults[State, Result] = _getResults()

  override def mapWithHistory[NewResult](f: ReadyParseResult[State, Result] => ReadyParseResult[State, NewResult], oldHistory: History) =
    new DelayedParseResult(remainder, this.history ++ oldHistory, () => {
      val intermediate = this.results
      intermediate.mapWithHistory(f, oldHistory)
    })

  override def mapReady[NewResult](f: ReadyParseResult[State, Result] => ReadyParseResult[State, NewResult], uniform: Boolean): DelayedParseResult[State, NewResult] =
    new DelayedParseResult(remainder, this.history, () => {
      val intermediate = this.results
      intermediate.mapReady(f, uniform)
    })

  override def flatMapReady[NewResult](f: ReadyParseResult[State, Result] => ParseResults[State, NewResult], uniform: Boolean) =
    singleResult(new DelayedParseResult(remainder, this.history, () => {
      val intermediate = this.results
      intermediate.flatMapReady(f, uniform)
    }))

  override def offset = remainder.position
}

case class RecursiveParseResult[State, SeedResult, +Result](
  get: ParseResults[State, SeedResult] => ParseResults[State, Result]) {

  def compose[NewResult](f: ParseResults[State, Result] => ParseResults[State, NewResult]):
    RecursiveParseResult[State, SeedResult, NewResult] = {

    RecursiveParseResult[State, SeedResult, NewResult](r => f(get(r)))
  }
}

case class ReadyParseResult[State, +Result](resultOption: Option[Result], remainder: InputGen[State], history: History)
  extends LazyParseResult[State, Result] {

  val originalScore = (if (history.flawed) 0 else 10000) + history.score
  override val score = 10000 + originalScore

  override def map[NewResult](f: Result => NewResult): ReadyParseResult[State, NewResult] = {
    ReadyParseResult(resultOption.map(f), remainder, history)
  }

  override def mapWithHistory[NewResult](f: ReadyParseResult[State, Result] => ReadyParseResult[State, NewResult], oldHistory: History) = {
    val newReady = f(this)
    ReadyParseResult(newReady.resultOption, newReady.remainder, newReady.history ++ oldHistory)
  }

  override def mapReady[NewResult](f: ReadyParseResult[State, Result] => ReadyParseResult[State, NewResult], uniform: Boolean):
    ReadyParseResult[State, NewResult] = f(this)

  override def flatMapReady[NewResult](f: ReadyParseResult[State, Result] => ParseResults[State, NewResult], uniform: Boolean) = f(this)

  override def offset = remainder.position
}