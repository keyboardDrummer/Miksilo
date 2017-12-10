package core.deltas

import core.deltas.node.Key

trait Delta extends Contract with Key {

  def inject(language: Language): Unit = {  }

  def description: String
}
