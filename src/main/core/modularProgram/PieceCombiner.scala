package core.modularProgram

trait StoppableState {
  var stop = false
}
object PieceCombiner {

  def combineAndExecute[T <: StoppableState](state: T, pieces: Seq[PieceOfCode[T]]) : Unit = {
    for (piece <- pieces)
      piece.enter(state)

    for (piece <- pieces.reverse) {
      piece.leave(state)
      if (state.stop)
        return
    }
  }

}
