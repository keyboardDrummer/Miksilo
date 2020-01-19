package deltas.bytecode.extraBooleanInstructions

import core.deltas._
import core.deltas.path.{NodePath, PathRoot}
import core.language.Compilation
import core.language.node.Node
import deltas.bytecode.ByteCodeSkeleton.ClassFile
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.attributes.CodeAttributeDelta.CodeAttribute
import deltas.bytecode.{ByteCodeMethodInfo, ByteCodeSkeleton}

object ExpandVirtualInstructionsDelta extends DeltaWithPhase {

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  val expandInstruction = new ShapeProperty[ExpandInstruction]

  override def transformProgram(program: Node, compilation: Compilation): Unit = {

    val classFile: ClassFile[NodePath] = PathRoot.fromCompilation(compilation)
    val codeAnnotations = CodeAttributeDelta.getCodeAnnotations[NodePath](classFile)

    for (codeAnnotation <- codeAnnotations) {
      processCodeAnnotation(codeAnnotation)
    }

    def processCodeAnnotation(codeAnnotation: CodeAttribute[NodePath]): Unit = {
      val methodInfo = codeAnnotation.findAncestorShape(ByteCodeMethodInfo.Shape)
      val instructions = codeAnnotation.current.instructions
      val newInstructions: Seq[Node] = getNewInstructions(instructions, methodInfo)
      codeAnnotation(CodeAttributeDelta.Instructions) = newInstructions
    }

    def getNewInstructions(instructions: Seq[Node], methodInfo: Node): Seq[Node] = {
      val expandInstructions = expandInstruction.get(compilation)
      instructions.flatMap(instruction => {
        expandInstructions.get(instruction.shape).fold(
          Seq(instruction))(
          expand => expand.expand(instruction, methodInfo, compilation))
      })
    }
  }

  override def description: String = "Defines a phase where custom bytecode instructions can expand into one or several actual bytecode instructions."
}
