package core.parsers.editorParsers

import core.parsers.core.TextPointer

object History {
  def empty = SpotlessHistory(0)
  def error(error: ParseError) = SingleError(0, error)
  def success(start: TextPointer, end: TextPointer, value: Any,
              successScore: Double = History.successValue): History = SpotlessHistory(successScore)

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

trait History {
  def canMerge: Boolean
  def spotless: Boolean = !flawed
  def flawed: Boolean
  def addError(newHead: ParseError): History
  def ++(right: History): History
  def score: Double
  def addSuccess(successScore: Double): History
  def addSuccess(from: TextPointer, until: TextPointer, value: Any,
                 successScore: Double = History.successValue): History = addSuccess(successScore)

  def errors: Iterable[ParseError]

  override def toString = s"score: $score, errors: $errors"
}

case class SpotlessHistory(score: Double = 0) extends History {

  override def addError(newHead: ParseError) = SingleError(score, newHead)

  override def ++(right: History) = right.addSuccess(score)

  override def addSuccess(successScore: Double) = SpotlessHistory(score + successScore)

  override def flawed = false

  override def errors = Iterable.empty

  override def toString = "Spotless: " + score

  override def canMerge = false
}

case class SingleError(successScore: Double, error: ParseError) extends History {
  override def flawed = true

  override def addError(newHead: ParseError) = error.append(newHead) match {
    case None => FlawedHistory(score + newHead.score, newHead, Rose.empty, error)
    case Some(merged) => SingleError(successScore, merged)
  }

  override def ++(right: History) = right.addError(error).addSuccess(successScore)

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

case class FlawedHistory(score: Double,
                                lastError: ParseError,
                                middleErrors: Rose[ParseError],
                                firstError: ParseError)
  extends History {

  def addError(newHead: ParseError): History = {
    lastError.append(newHead) match {
      case None => FlawedHistory(score + newHead.score, newHead, Rose.node(Leaf(lastError), middleErrors), firstError)
      case Some(merged) => FlawedHistory(score - lastError.score + merged.score, merged, middleErrors, firstError)
    }
  }

  def ++(right: History): History = {
    right match {
      case withChoices: HistoryWithChoices => HistoryWithChoices(withChoices.choices, this ++ withChoices.inner)
      case _ =>

        val (rightWithLastScore, newMiddleErrors, newLast) = right.addError(firstError) match {
          case single: SingleError => (single.score, middleErrors, single.error)
          case flawed: FlawedHistory => (flawed.score, Rose.node(middleErrors, Leaf(flawed.lastError), flawed.middleErrors), flawed.firstError)
        }
        FlawedHistory(score - firstError.score + rightWithLastScore,
          lastError,
          newMiddleErrors,
          newLast)
    }
  }

  def addSuccess(successScore: Double): History = {
    FlawedHistory(score + successScore, lastError, middleErrors, firstError)
  }

  override def flawed = true

  override def errors = Seq.concat(Seq(firstError), middleErrors.values, Seq(lastError))

  override def canMerge = lastError.canMerge
}

case class HistoryWithChoices(choices: Seq[(TextPointer, Any)], inner: History = History.empty) extends History {

  override def canMerge = true

  override def flawed = inner.flawed

  override def addError(newHead: ParseError) = HistoryWithChoices(choices, inner.addError(newHead))

  override def ++(right: History) = right match {
    case withChoices: HistoryWithChoices =>
      HistoryWithChoices(choices ++ withChoices.choices, inner ++ withChoices.inner)
    case _ => HistoryWithChoices(choices, inner ++ right)
  }

  override def score = inner.score

  override def addSuccess(successScore: Double) = HistoryWithChoices(choices, inner.addSuccess(successScore))

  override def errors = inner.errors
}
