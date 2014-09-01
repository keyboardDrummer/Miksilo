package core.transformation

import core.grammarDocument.FailureG
import core.modularProgram.StoppableState
import core.transformation.grammars.{GrammarCatalogue, ProgramGrammar}

import scala.collection.mutable
import scala.util.Random

class TransformationState() extends StoppableState {
  val data: mutable.Map[Any, Any] = mutable.Map.empty
  val grammarCatalogue = new GrammarCatalogue
  var program: MetaObject = null

  def getUniqueLabel(prefix: String) = prefix + getGUID

  def getGUID: Long = Random.nextLong()

  grammarCatalogue.create(ProgramGrammar, FailureG)
}
