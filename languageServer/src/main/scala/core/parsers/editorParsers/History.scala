package core.parsers.editorParsers

object History {
  def empty[Input] = SpotlessHistory[Input](0)
  def error[Input](error: ParseError[Input]) = SingleError(0, error)
  def success[Input](start: Input, end: Input, value: Any,
              successScore: Double = History.successValue): History[Input] = SpotlessHistory(successScore)

  val successValue = 1.0

  val errorMultiplier = successValue * 2

  val indentationErrorPenalty = successValue * errorMultiplier * 10

  val genericErrorPenalty = successValue * errorMultiplier
  val missingInputPenalty = successValue * errorMultiplier
  val insertFallbackPenalty = missingInputPenalty * 0.9
  val insertDefaultPenalty = 1
  val failPenalty = 100

  val dropLength1Penalty = errorMultiplier * 3
  val dropLength2Penalty = dropLength1Penalty / 0.75
  val dropMaxPenalty = dropLength1Penalty * 10
  val dropLengthShift = (dropMaxPenalty + dropLength1Penalty - 2 * dropLength2Penalty) / (dropLength2Penalty - dropLength1Penalty)
  val dropReduction: Double = (dropMaxPenalty - dropLength1Penalty) * (1 + dropLengthShift)

  val endOfSourceInsertion = 2 // Inserting at the end of the file should be cheap, since it's likely that you're missing input there.

}

trait History[Input] {
  def canMerge: Boolean
  def flawed: Boolean
  def addError(newHead: ParseError[Input]): History[Input]
  def ++(right: History[Input]): History[Input]
  def score: Double
  def addSuccess(successScore: Double): History[Input]
  def addSuccess(start: Input, end: Input, value: Any,
                 successScore: Double = History.successValue): History[Input] = addSuccess(successScore)

  def errors: Iterable[ParseError[Input]]

  override def toString = errors.toString()
}

case class SpotlessHistory[Input](score: Double = 0) extends History[Input] {

  override def addError(newHead: ParseError[Input]) = SingleError(score, newHead)

  override def ++(right: History[Input]) = right.addSuccess(score)

  override def addSuccess(successScore: Double) = SpotlessHistory(score + successScore)

  override def flawed = false

  override def errors = Iterable.empty

  override def toString = "Spotless: " + score

  override def canMerge = false
}

case class SingleError[Input](successScore: Double, error: ParseError[Input]) extends History[Input] {
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

  def node[Value](children: Rose[Value]*) = Node[Value](children.filter(v => v != null).toArray:_*)
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

case class FlawedHistory[Input](score: Double, firstError: ParseError[Input],
                                middleErrors: Rose[ParseError[Input]],
                                lastError: ParseError[Input])
  extends History[Input] {

  def addError(newHead: ParseError[Input]): History[Input] = {
    firstError.append(newHead) match {
      case None => FlawedHistory(score + newHead.score, newHead, Rose.node(Leaf(firstError), middleErrors), lastError)
      case Some(merged) => FlawedHistory(score - firstError.score + merged.score, merged, middleErrors, lastError)
    }
  }

  def ++(right: History[Input]): History[Input] = {
    val (rightScore, rightDropRight1, rightLast) = right.addError(lastError) match {
      case single: SingleError[Input] => (single.score, null, single.error)
      case flawed: FlawedHistory[Input] => (flawed.score, Rose.node(Leaf(flawed.firstError), flawed.middleErrors), flawed.lastError)
    }
    FlawedHistory(score - lastError.score + rightScore,
      firstError,
      if (rightDropRight1 == null) middleErrors else Rose.node(middleErrors, rightDropRight1),
      rightLast)
  }

  def addSuccess(successScore: Double): History[Input] = {
    FlawedHistory(score + successScore, firstError, middleErrors, lastError)
  }

  override def flawed = true

  override def errors = Seq.concat(Seq(firstError), middleErrors.values, Seq(lastError))

  override def canMerge = firstError.canMerge
}