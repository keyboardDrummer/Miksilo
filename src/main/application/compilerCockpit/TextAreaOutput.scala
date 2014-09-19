package application.compilerCockpit

import core.transformation.TransformationState

class TextAreaOutput(setText: String => Unit) extends OutputOption {

   override def inject(state: TransformationState): Unit = {}

   override def leave(state: TransformationState): Unit = {
     val outputOption = OutputOption.getOutput(state)
     setText(outputOption.getOrElse("No output."))
     state.stop = true
   }

   override def toString = "Output to text area"
 }
