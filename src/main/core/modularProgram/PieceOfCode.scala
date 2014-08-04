package core.modularProgram

import scala.collection.mutable

trait PieceOfCode {
  def enter(state: mutable.HashMap[PieceOfCode, Any])

  def leave(state: mutable.HashMap[PieceOfCode, Any])
}
