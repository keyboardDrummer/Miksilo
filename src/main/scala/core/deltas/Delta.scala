package core.deltas

import core.deltas.node.Key
import core.language.Language

trait Delta extends Contract with Key {

  def suffix = "Delta"

  def inject(language: Language): Unit = {  }

  def description: String
}
