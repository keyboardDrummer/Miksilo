package core.particles

import java.io.InputStream

import core.particles.grammars.LanguageGrammars
import core.particles.node.Node

import scala.collection.mutable
import scala.util.Random

case class Phase(name: String, description: String, action: Compilation => Unit)

class Language {

  val data: mutable.Map[Any, Any] = mutable.Map.empty
  val grammars = new LanguageGrammars

  var compilerPhases: List[Phase] = List.empty
  var parse: InputStream => Node = _

  var random = new Random(0)
  def getUniqueLabel(prefix: String): String = prefix + getGUID

  def getGUID: Long = random.nextLong()
}
