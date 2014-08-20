package application.compilerBuilder.compilerCockpit

import core.grammar.ParseException
import core.transformation.{MetaObject, TransformationsToPackrat, TransformationState}

class TextAreaInput(cockpit: CompilerCockpit) extends InputOption {

   override def enter(state: TransformationState): Unit = {}

   override def leave(state: TransformationState): Unit = {
     val inputStream = cockpit.inputTextArea.getText
     val manager = new TransformationsToPackrat()
     val parser = manager.buildParser(state.grammarCatalogue)

     val parseResult = parser(inputStream)
     if (!parseResult.successful)
       throw new ParseException(parseResult.toString)

     state.program = parseResult.get.asInstanceOf[MetaObject]
   }

   override def toString = "TextAreaInput"
 }
