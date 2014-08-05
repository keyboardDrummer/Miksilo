package core.transformation

import core.grammar.ParseException
import core.modularProgram.PieceOfCode

import scala.reflect.io.File

class ReadUsingGrammarTransformation(input: File) extends PieceOfCode[TransformationState] {

  override def enter(state: TransformationState): Unit = {}

  override def leave(state: TransformationState): Unit = {
    val inputStream = File(input).slurp()
    val manager = new TransformationsToPackrat()
    val parser = manager.buildParser(state.grammarCatalogue)

    val parseResult = parser(inputStream)
    if (!parseResult.successful)
      throw new ParseException(parseResult.toString)

    state.program = parseResult.get.asInstanceOf[MetaObject]
  }
}
