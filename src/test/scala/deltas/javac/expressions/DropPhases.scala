package deltas.javac.expressions

import core.deltas.Delta
import core.language.Language

case class DropPhases(amount: Int) extends Delta {
  override def description: String = "Drop n phases"

  override def inject(language: Language): Unit = {
    language.compilerPhases = language.compilerPhases.drop(amount)
  }
}
