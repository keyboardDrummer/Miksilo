package core.parsers.editorParsers

object History {
  def empty[Input] = SpotlessHistory[Input](0)
  def error[Input](error: ParseError[Input]) = SingleError(0, error)

  val successValue = 1.0

  val errorMultiplier = successValue * 2

  val genericErrorPenalty = successValue
  val insertLiteralPenalty = successValue * errorMultiplier
  val insertRegexPenalty = successValue * errorMultiplier
  val insertDefaultPenalty = 1
  val failPenalty = 1 // TODO settings this too high kills the tests. I think withDefault should work differently.

  val dropLength1Penalty = errorMultiplier * 3
  val dropLength2Penalty = dropLength1Penalty / 0.75
  val dropMaxPenalty = dropLength1Penalty * 10
  val dropLengthShift = (dropMaxPenalty + dropLength1Penalty - 2 * dropLength2Penalty) / (dropLength2Penalty - dropLength1Penalty)
  val dropReduction: Double = (dropMaxPenalty - dropLength1Penalty) * (1 + dropLengthShift)

  val endOfSourceInsertion = 2 // Inserting at the end of the file should be cheap, since it's likely that you're missing input there.

}

trait History[Input] {
  def flawed: Boolean
  def addError(newHead: ParseError[Input]): History[Input]
  def ++(right: History[Input]): History[Input]
  def score: Double
  def addSuccess(successScore: Double): History[Input]
  def addSuccess(start: Input, end: Input, value: Any,
                 successScore: Double = History.successValue): History[Input] = addSuccess(successScore)

  def errors: Iterable[ParseError[Input]]
}

case class SpotlessHistory[Input](score: Double = 0) extends History[Input] {
  override def addError(newHead: ParseError[Input]) = SingleError(score, newHead)

  override def ++(right: History[Input]) = right.addSuccess(score)

  override def addSuccess(successScore: Double) = SpotlessHistory(score + successScore)

  override def flawed = false

  override def errors = Iterable.empty
}

case class SingleError[Input](successScore: Double, error: ParseError[Input]) extends History[Input] {
  override def flawed = true

  override def addError(newHead: ParseError[Input]) = error.append(newHead) match {
    case None => FlawedHistory(score + newHead.score, newHead, Iterable.empty, error)
    case Some(merged) => SingleError(successScore, merged)
  }

  override def ++(right: History[Input]) = right.addError(error).addSuccess(successScore)

  override def addSuccess(successScore: Double) = SingleError(this.successScore + successScore, error)

  override def errors = Iterable(error)

  override def score = successScore + error.score
}

case class FlawedHistory[Input](score: Double, firstError: ParseError[Input],
                                middleErrors: Iterable[ParseError[Input]],
                                lastError: ParseError[Input])
  extends History[Input] {

  def addError(newHead: ParseError[Input]): History[Input] = {
    firstError.append(newHead) match {
      case None => FlawedHistory(score + newHead.score, newHead, Iterable.concat(Iterable(firstError), middleErrors), lastError)
      case Some(merged) => FlawedHistory(score - firstError.score + merged.score, merged, middleErrors, lastError)
    }
  }

  def ++(right: History[Input]): History[Input] = {
    val (rightScore, rightDropRight1, rightLast) = right.addError(lastError) match {
      case single: SingleError[Input] => (single.score, Iterable.empty, single.error)
      case flawed: FlawedHistory[Input] => (flawed.score, Iterable.concat(Iterable(flawed.firstError), flawed.middleErrors), flawed.lastError)
    }
    FlawedHistory(score - lastError.score + rightScore,
      firstError,
      Iterable.concat(middleErrors, rightDropRight1),
      rightLast)
  }

  def addSuccess(successScore: Double): History[Input] = {
    FlawedHistory(score + successScore, firstError, middleErrors, lastError)
  }

  override def flawed = true

  override def errors = Iterable.concat(Iterable(firstError), middleErrors, Iterable(lastError))
}