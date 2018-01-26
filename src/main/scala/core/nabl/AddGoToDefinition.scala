package core.nabl

import core.deltas.Delta
import core.language.Language

class AddGoToDefinition extends Delta {


  override def inject(language: Language): Unit = {
//    language.capabilities += new GotoDefinition {
//      override def apply(position: Position): Range = ???
//    }
    super.inject(language)
  }

  override def description: String = "Adds the go to definition capability"
}
