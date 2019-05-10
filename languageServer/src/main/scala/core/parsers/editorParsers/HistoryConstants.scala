package core.parsers.editorParsers

object HistoryConstants {
  val successValue = 1
  val dropMaxPenalty = successValue // Has to be at or below successValue, otherwise splitting a long drop with a success will reduce the score.
  val dropStartingPenalty = 0.9 // Let's try to keep it close to successValue

  val genericErrorPenalty = dropStartingPenalty
  val insertLiteralPenalty = dropStartingPenalty * 5
  val insertRegexPenalty = dropStartingPenalty * 5
  val failPenalty = 5

  val endOfSourceInsertion = 5 // Inserting at the end of the file should be cheap, since it's likely that you're missing input there.

  val dropReduction = dropMaxPenalty - dropStartingPenalty
}
