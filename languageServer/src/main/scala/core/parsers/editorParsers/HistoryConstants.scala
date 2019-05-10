package core.parsers.editorParsers

object HistoryConstants {
  val successValue = 1.0

  val dropMaxPenalty = successValue * 10
  val dropLength2Penalty = successValue * 3
  val dropLength1Penalty = dropLength2Penalty * 0.75
  val dropLengthShift = (dropMaxPenalty + dropLength1Penalty - 2 * dropLength2Penalty) / (dropLength2Penalty - dropLength1Penalty)
  val dropReduction: Double = (dropMaxPenalty - dropLength1Penalty) * (1 + dropLengthShift)

  val genericErrorPenalty = successValue
  val insertLiteralPenalty = successValue * 2
  val insertRegexPenalty = successValue * 2
  val failPenalty = 1 // TODO settings this too high kills the tests. I think withDefault should work differently.

  val endOfSourceInsertion = 2 // Inserting at the end of the file should be cheap, since it's likely that you're missing input there.

}
