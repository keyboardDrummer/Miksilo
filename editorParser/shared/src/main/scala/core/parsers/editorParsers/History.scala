package core.parsers.editorParsers

import core.parsers.core.ParseInput

object History {
  def empty[Input <: ParseInput] = SpotlessHistory[Input](0)
  def error[Input <: ParseInput](error: ParseError[Input]) = SingleError(0, error)
  def success[Input <: ParseInput](start: Input, end: Input, value: Any,
              successScore: Double = History.successValue): History[Input] = SpotlessHistory(successScore)

  val successValue = 1.0

  val errorMultiplier = successValue * 4

  val indentationErrorPenalty = successValue * errorMultiplier * 10

  val missingInputPenalty = successValue * errorMultiplier
  val insertFallbackPenalty = missingInputPenalty * 0.9
  val insertDefaultPenalty = 1
  val failPenalty = 1000000

  val dropLength1Penalty = errorMultiplier * 3
  val dropLength2Penalty = dropLength1Penalty / 0.75
  val dropMaxPenalty = dropLength1Penalty * 10
  val dropLengthShift = (dropMaxPenalty + dropLength1Penalty - 2 * dropLength2Penalty) / (dropLength2Penalty - dropLength1Penalty)
  val dropReduction: Double = (dropMaxPenalty - dropLength1Penalty) * (1 + dropLengthShift)
}

trait History[Input <: ParseInput] {
  def canMerge: Boolean
  def spotless: Boolean = !flawed
  def flawed: Boolean
  def addError(newHead: ParseError[Input]): History[Input]
  def ++(right: History[Input]): History[Input]
  def score: Double
  def addSuccess(successScore: Double): History[Input]
  def addSuccess(start: Input, end: Input, value: Any,
                 successScore: Double = History.successValue): History[Input] = addSuccess(successScore)

  def errors: Iterable[ParseError[Input]]

  override def toString = s"score: $score, errors: $errors"
}

case class SpotlessHistory[Input <: ParseInput](score: Double = 0) extends History[Input] {

  override def addError(newHead: ParseError[Input]) = SingleError(score, newHead)

  override def ++(right: History[Input]) = right.addSuccess(score)

  override def addSuccess(successScore: Double) = SpotlessHistory(score + successScore)

  override def flawed = false

  override def errors = Iterable.empty

  override def toString = "Spotless: " + score

  override def canMerge = false
}

case class SingleError[Input <: ParseInput](successScore: Double, error: ParseError[Input]) extends History[Input] {
  override def flawed = true

  override def addError(newHead: ParseError[Input]) = error.append(newHead) match {
    case None => FlawedHistory(score + newHead.score, newHead, Rose.empty, error)
    case Some(merged) => SingleError(successScore, merged)
  }

  override def ++(right: History[Input]) = right.addError(error).addSuccess(successScore)

  override def addSuccess(successScore: Double) = SingleError(this.successScore + successScore, error)

  override def errors = Iterable(error)

  override def score = successScore + error.score

  override def canMerge = error.canMerge
}

object Rose {
  val empty = Node[Nothing]()

  def node[Value](children: Rose[Value]*) = Node[Value](children.filter(v => v != null).toIndexedSeq:_*)
}

trait Rose[+Value] {
  def values: Seq[Value]
  def map[NewValue](f: Value => NewValue): Rose[NewValue]
  override def toString = values.toString()
}

case class Leaf[+Value](value: Value) extends Rose[Value] {
  override def values = Seq(value)

  override def map[NewValue](f: Value => NewValue) = Leaf(f(value))
}

case class Node[+Value](children: Rose[Value]*) extends Rose[Value] {
  def values = children.flatMap(child => child.values)

  override def map[NewValue](f: Value => NewValue) = Node(children.map(r => r.map(f)):_*)
}

case class FlawedHistory[Input <: ParseInput](score: Double,
                                lastError: ParseError[Input],
                                middleErrors: Rose[ParseError[Input]],
                                firstError: ParseError[Input])
  extends History[Input] {

  def addError(newHead: ParseError[Input]): History[Input] = {
    lastError.append(newHead) match {
      case None => FlawedHistory(score + newHead.score, newHead, Rose.node(Leaf(lastError), middleErrors), firstError)
      case Some(merged) => FlawedHistory(score - lastError.score + merged.score, merged, middleErrors, firstError)
    }
  }

  def ++(right: History[Input]): History[Input] = {
    right match {
      case withChoices: HistoryWithChoices[Input] => HistoryWithChoices(withChoices.choices, this ++ withChoices.inner)
      case _ =>

        val (rightWithLastScore, newMiddleErrors, newLast) = right.addError(firstError) match {
          case single: SingleError[Input] => (single.score, middleErrors, single.error)
          case flawed: FlawedHistory[Input] => (flawed.score, Rose.node(middleErrors, Leaf(flawed.lastError), flawed.middleErrors), flawed.firstError)
        }
        FlawedHistory(score - firstError.score + rightWithLastScore,
          lastError,
          newMiddleErrors,
          newLast)
    }
  }

  def addSuccess(successScore: Double): History[Input] = {
    FlawedHistory(score + successScore, lastError, middleErrors, firstError)
  }

  override def flawed = true

  override def errors = Seq.concat(Seq(firstError), middleErrors.values, Seq(lastError))

  override def canMerge = lastError.canMerge
}

case class HistoryWithChoices[Input <: ParseInput](choices: Seq[(Input, Any)], inner: History[Input] = History.empty[Input]) extends History[Input] {

  override def canMerge = true

  override def flawed = inner.flawed

  override def addError(newHead: ParseError[Input]) = HistoryWithChoices(choices, inner.addError(newHead))

  override def ++(right: History[Input]) = right match {
    case withChoices: HistoryWithChoices[Input] =>
      HistoryWithChoices(choices ++ withChoices.choices, inner ++ withChoices.inner)
    case _ => HistoryWithChoices(choices, inner ++ right)
  }

  override def score = inner.score

  override def addSuccess(successScore: Double) = HistoryWithChoices(choices, inner.addSuccess(successScore))

  override def errors = inner.errors
}
