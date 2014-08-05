package core.transformation

import core.grammar.FailureG

import scala.collection.mutable
import scala.util.Random

class TransformationState {
  val data: mutable.Map[Contract, Any] = mutable.Map.empty
  val grammarCatalogue = new GrammarCatalogue
  var program: MetaObject = null

  def getUniqueLabel(prefix: String) = prefix + getGUID

  def getGUID: Long = Random.nextLong()

  grammarCatalogue.create(ProgramGrammar, FailureG)
}
