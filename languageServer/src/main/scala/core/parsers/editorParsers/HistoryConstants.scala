package core.parsers.editorParsers

object HistoryConstants {
  val successValue = 1.0

  val dropMaxPenalty = successValue * 10
  val dropLength1Penalty = 1.0
  val dropLength2Penalty = 1.9 * dropLength1Penalty
  val dropLengthShift = (dropMaxPenalty + dropLength1Penalty - 2 * dropLength2Penalty) / (dropLength2Penalty - dropLength1Penalty)
  val dropReduction: Double = (dropMaxPenalty - dropLength1Penalty) * (1 + dropLengthShift)

  val genericErrorPenalty = successValue
  val insertLiteralPenalty = successValue * 2
  val insertRegexPenalty = successValue * 2
  val failPenalty = 2

  val endOfSourceInsertion = 2 // Inserting at the end of the file should be cheap, since it's likely that you're missing input there.

}
