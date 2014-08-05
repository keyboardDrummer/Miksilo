package core.modularProgram

object PieceCombiner {

  def combineAndExecute[T](state: T, pieces: Seq[PieceOfCode[T]]) = {
    for (piece <- pieces)
      piece.enter(state)

    for (piece <- pieces.reverse)
      piece.leave(state)
  }

}
