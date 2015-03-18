package core.particles

import core.biGrammar.BiFailure
import core.grammar.ParseException
import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}
import core.particles.node.MetaObject

import scala.collection.mutable
import scala.util.Random

class CompilationState {
  var output: String = null
  val data: mutable.Map[Any, Any] = mutable.Map.empty
  val grammarCatalogue = new GrammarCatalogue
  grammarCatalogue.create(ProgramGrammar, BiFailure)
  var program: MetaObject = null
  var compilerPhases: List[() => Unit] = List.empty

  def getUniqueLabel(prefix: String) = prefix + getGUID

  def getGUID: Long = Random.nextLong()


  def parseString(input: String): Unit = {
    val manager = new ParticlesToParserConverter()
    val parser = manager.buildParser(grammarCatalogue)

    val parseResult = parser(input)
    if (!parseResult.successful)
      throw new ParseException(parseResult.toString)

    if(!parseResult.next.atEnd)
      throw new ParseException("Did not parse until end.")

    program = parseResult.get.asInstanceOf[MetaObject]
  }

  def runPhases() = {
    for(phase <- compilerPhases)
      phase()
  }
}
