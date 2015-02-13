package core.transformation

import core.grammar.ParseException
import core.grammarDocument.BiFailure
import core.transformation.grammars.{GrammarCatalogue, ProgramGrammar}

import scala.collection.mutable
import scala.util.Random

class TransformationState {
  var output: String = null
  val data: mutable.Map[Any, Any] = mutable.Map.empty
  val grammarCatalogue = new GrammarCatalogue
  var program: MetaObject = null
  var compilerPhases: List[() => Unit] = List.empty

  def getUniqueLabel(prefix: String) = prefix + getGUID

  def getGUID: Long = Random.nextLong()

  grammarCatalogue.create(ProgramGrammar, BiFailure)

  def parseString(input: String): Unit = {
    val manager = new TransformationsToPackrat()
    val parser = manager.buildParser(grammarCatalogue)

    val parseResult = parser(input)
    if (!parseResult.successful)
      throw new ParseException(parseResult.toString)

    program = parseResult.get.asInstanceOf[MetaObject]
  }

  def runPhases() = {
    for(phase <- compilerPhases)
      phase()
  }
}
