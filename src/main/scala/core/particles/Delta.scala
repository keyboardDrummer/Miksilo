package core.particles

import core.particles.node.Key

trait Delta extends Contract with Key {

  def inject(language: Language): Unit = {  }

  final def dependencies2: Set[Delta] = dependencies.collect({case x: Delta => x})

  def description: String
}
