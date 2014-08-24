package application.compilerCockpit

import core.transformation.TransformationState

class TextAreaOutput(setText: String => Unit) extends OutputOption {

   override def enter(state: TransformationState): Unit = {}

   override def leave(state: TransformationState): Unit = {
     setText(OutputOption.getOutput(state))
     state.stop = true
   }

   override def toString = "Output to text area"
 }
