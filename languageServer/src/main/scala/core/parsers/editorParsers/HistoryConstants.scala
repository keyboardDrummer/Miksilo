package core.parsers.editorParsers

object HistoryConstants {
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
