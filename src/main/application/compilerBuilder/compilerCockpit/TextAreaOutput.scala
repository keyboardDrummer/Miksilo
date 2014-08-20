package application.compilerBuilder.compilerCockpit

import core.transformation.TransformationState

class TextAreaOutput(cockpit: CompilerCockpit) extends OutputOption {

   override def enter(state: TransformationState): Unit = {}

   override def leave(state: TransformationState): Unit = {
     cockpit.outputTextArea.setText(OutputOption.getOutput(state))
   }

   override def toString = "TextAreaOutput"
 }
