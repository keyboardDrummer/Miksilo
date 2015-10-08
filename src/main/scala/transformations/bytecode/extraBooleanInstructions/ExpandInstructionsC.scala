package transformations.bytecode.extraBooleanInstructions

import core.particles._
import core.particles.node.Node
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.additions.LabelledTargets
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.integers.SmallIntegerConstantC
import transformations.bytecode.simpleBytecode.InferredStackFrames

import scala.collection.mutable

object ExpandInstructionsC extends ParticleWithPhase with WithState {

  def lessThanInstruction = CodeAttribute.instruction(LessThanInstructionKey)

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

        val replacement = instruction.clazz match {
          case LessThanInstructionKey =>
            val falseStartLabel = state.getUniqueLabel("falseStart")
            val endLabel = state.getUniqueLabel("end")
            Seq(LabelledTargets.ifIntegerCompareLess(falseStartLabel),
              SmallIntegerConstantC.integerConstant(0),
              LabelledTargets.goTo(endLabel),
              InferredStackFrames.label(falseStartLabel),
              SmallIntegerConstantC.integerConstant(1),
              InferredStackFrames.label(endLabel))
          case _ => Seq(instruction)
        }
        newInstructions ++= expandOption.fold(Seq(instruction))(expand => expand(instruction))
      }

      newInstructions

    }
  }

  object LessThanInstructionKey

  override def description: String = "Defines a phase where custom bytecode instructions can expand into one or several actual bytecode instructions."

  override def createState: State = new State()
}
