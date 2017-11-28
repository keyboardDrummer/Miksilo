package deltas.bytecode.extraBooleanInstructions

import core.deltas._
import core.deltas.node.Node
import core.deltas.path.{Path, PathRoot}
import deltas.bytecode.{ByteCodeMethodInfo, ByteCodeSkeleton}
import deltas.bytecode.additions.LabelledLocations
import deltas.bytecode.attributes.CodeAttribute
import deltas.bytecode.attributes.CodeAttribute.CodeWrapper
import deltas.bytecode.coreInstructions.integers.SmallIntegerConstantDelta
import deltas.bytecode.extraBooleanInstructions.LessThanInstructionC.LessThanInstructionKey
import deltas.bytecode.simpleBytecode.InferredStackFrames
import deltas.javac.classes.MethodInfo

import scala.collection.mutable

object ExpandVirtualInstructionsC extends DeltaWithPhase with WithLanguageRegistry {

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  class Registry {
    val expandInstruction = new ClassRegistry[ExpandInstruction]()
  }

  override def transformProgram(program: Node, state: Compilation): Unit = {

    val clazz = program
    val codeAnnotations: Seq[Path] = CodeAttribute.getCodeAnnotations(PathRoot(clazz))

    for (codeAnnotation <- codeAnnotations) {
      processCodeAnnotation(codeAnnotation)
    }

    def processCodeAnnotation(codeAnnotation: CodeWrapper[Path]): Unit = {
      val methodInfo = codeAnnotation.ancestors.find(p => p.current.clazz == ByteCodeMethodInfo.MethodInfoKey).get
      val instructions = codeAnnotation.instructions
      val newInstructions: Seq[Node] = getNewInstructions(instructions, methodInfo)
      codeAnnotation(CodeAttribute.Instructions) = newInstructions
    }

    def getNewInstructions(instructions: Seq[Node], methodInfo: Node) = {

      var newInstructions = mutable.ArrayBuffer[Node]()

      for (instruction <- instructions) {

        val expandOption = getRegistry(state.language).expandInstruction.get(instruction.clazz)
        newInstructions ++= expandOption.fold(Seq(instruction))(expand => expand.expand(instruction, methodInfo, state))
      }

      newInstructions

    }
  }

  override def description: String = "Defines a phase where custom bytecode instructions can expand into one or several actual bytecode instructions."

  override def createRegistry: Registry = new Registry()
}
