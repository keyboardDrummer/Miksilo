package core.deltas

import java.io.InputStream

import core.deltas.grammars.LanguageGrammars
import core.deltas.node.Node

import scala.collection.mutable

case class Phase(key: Delta, action: Compilation => Unit)

class Language {
  val data: mutable.Map[Any, Any] = mutable.Map.empty
  val grammars = new LanguageGrammars

  var compilerPhases: List[Phase] = List.empty
  var parse: InputStream => Node = _
}
