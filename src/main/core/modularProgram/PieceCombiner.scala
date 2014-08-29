package core.modularProgram

trait StoppableState {
  var stop = false
}
object PieceCombiner {

  def combineAndExecute[T <: StoppableState](state: T, pieces: Seq[PieceOfCode[T]]) : Unit = {
    var enteredPieces = List.empty[PieceOfCode[T]]
    for (piece <- pieces) {
      if (!state.stop) {
        piece.enter(state)
        enteredPieces ::= piece
      }
    }

    state.stop = false

    for (piece <- enteredPieces) {
      piece.leave(state)
      if (state.stop)
        return
    }
  }

}
