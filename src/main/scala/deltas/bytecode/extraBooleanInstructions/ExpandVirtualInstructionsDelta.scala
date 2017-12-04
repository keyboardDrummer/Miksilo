package deltas.bytecode.extraBooleanInstructions

import core.deltas._
import core.deltas.node.Node
import core.deltas.path.{Path, PathRoot}
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.attributes.CodeAttributeDelta.{CodeAttribute, Instructions}
import deltas.bytecode.{ByteCodeMethodInfo, ByteCodeSkeleton}

object ExpandVirtualInstructionsDelta extends DeltaWithPhase with WithLanguageRegistry {

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  class Registry {
    val expandInstruction = new ClassRegistry[ExpandInstruction]()
  }

  override def transformProgram(program: Node, compilation: Compilation): Unit = {

    val clazz = program
    val codeAnnotations = CodeAttributeDelta.getCodeAnnotations[Path](PathRoot(clazz))

    for (codeAnnotation <- codeAnnotations) {
      processCodeAnnotation(codeAnnotation)
    }

    def processCodeAnnotation(codeAnnotation: CodeAttribute[Path]): Unit = {
      val methodInfo = codeAnnotation.findAncestorClass(ByteCodeMethodInfo.MethodInfoKey)
      val instructions = codeAnnotation.current.instructions
      val newInstructions: Seq[Node] = getNewInstructions(instructions, methodInfo)
      codeAnnotation(CodeAttributeDelta.Instructions) = newInstructions
    }

    def getNewInstructions(instructions: Seq[Node], methodInfo: Node): Seq[Node] = {
      val expandInstructions = getRegistry(compilation.language).expandInstruction
      instructions.flatMap(instruction => {
        expandInstructions.get(instruction.clazz).fold(
          Seq(instruction))(
          expand => expand.expand(instruction, methodInfo, compilation))
      })
    }
  }

  override def description: String = "Defines a phase where custom bytecode instructions can expand into one or several actual bytecode instructions."

  override def createRegistry: Registry = new Registry()
}
