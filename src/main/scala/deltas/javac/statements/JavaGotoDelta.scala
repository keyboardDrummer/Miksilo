package deltas.javac.statements

import core.deltas.{Delta, Language}

object JavaGotoDelta extends Delta {

  override def inject(state: Language): Unit = {
    JustJavaGoto.inject(state)
    JustJavaLabel.inject(state)
    super.inject(state)
  }

  override def description: String = "Adds goto and label statements"
}
