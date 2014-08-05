package core.modularProgram

trait PieceOfCode[T] {
  def enter(state: T)

  def leave(state: T)
}
