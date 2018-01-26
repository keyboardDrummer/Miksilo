package deltas.javac.statements

import core.deltas.{Contract, Delta}
import core.language.Language

object JavaGotoDelta extends Delta {

  override def description: String = "Adds goto and label statements"

  override def dependencies: Set[Contract] = JustJavaGoto.dependencies ++ JustJavaLabel.dependencies

  override def inject(state: Language): Unit = {
    JustJavaGoto.inject(state)
    JustJavaLabel.inject(state)
    super.inject(state)
  }
}
