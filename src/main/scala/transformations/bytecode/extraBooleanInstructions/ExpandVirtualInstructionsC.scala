package transformations.bytecode.extraBooleanInstructions

import core.particles._
import core.particles.node.Node
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.additions.LabelledLocations
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.integers.SmallIntegerConstantC
import transformations.bytecode.extraBooleanInstructions.LessThanInstructionC.LessThanInstructionKey
import transformations.bytecode.simpleBytecode.InferredStackFrames

import scala.collection.mutable

object ExpandVirtualInstructionsC extends ParticleWithPhase with WithState {

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  class State {
    val expandInstruction = new ClassRegistry[Node => Seq[Node]]()
  }

  override def transform(program: Node, state: CompilationState): Unit = {

    val clazz = program
    val codeAnnotations: Seq[Node] = CodeAttribute.getCodeAnnotations(clazz)

    for (codeAnnotation <- codeAnnotations) {
      processCodeAnnotation(codeAnnotation)
    }

    def processCodeAnnotation(codeAnnotation: Node): Option[Any] = {
      val instructions = CodeAttribute.getCodeInstructions(codeAnnotation)
      val newInstructions: Seq[Node] = getNewInstructions(instructions)
      codeAnnotation(CodeAttribute.CodeInstructionsKey) = newInstructions
    }

    def getNewInstructions(instructions: Seq[Node]) = {

      var newInstructions = mutable.ArrayBuffer[Node]()

      for (instruction <- instructions) {

        val expandOption = getState(state).expandInstruction.get(instruction.clazz)
        newInstructions ++= expandOption.fold(Seq(instruction))(expand => expand(instruction))
      }

      newInstructions

    }
  }

  override def description: String = "Defines a phase where custom bytecode instructions can expand into one or several actual bytecode instructions."

  override def createState: State = new State()
}
