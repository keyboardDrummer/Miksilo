package core.transformation.grammars

import core.grammar.ParseException
import core.modularProgram.PieceOfCode
import core.transformation.{MetaObject, TransformationState, TransformationsToPackrat}

import scala.reflect.io.File

object ReadUsingGrammarTransformation extends PieceOfCode[TransformationState] {

  override def enter(state: TransformationState): Unit = {}

  override def leave(state: TransformationState): Unit = {
    val input = state.data(InputFile).asInstanceOf[File]
    val inputStream = File(input).slurp()
    val manager = new TransformationsToPackrat()
    val parser = manager.buildParser(state.grammarCatalogue)

    val parseResult = parser(inputStream)
    if (!parseResult.successful)
      throw new ParseException(parseResult.toString)

    state.program = parseResult.get.asInstanceOf[MetaObject]
  }

  object InputFile

}
