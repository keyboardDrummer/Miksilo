package miksilo.modularLanguages.deltas.bytecode.extraBooleanInstructions

import miksilo.modularLanguages.core.deltas._
import miksilo.modularLanguages.core.deltas.path.{NodePath, PathRoot}
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.bytecode.ByteCodeSkeleton.ClassFile
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta.CodeAttribute
import miksilo.modularLanguages.deltas.bytecode.{ByteCodeMethodInfo, ByteCodeSkeleton}

object ExpandVirtualInstructionsDelta extends DeltaWithPhase {

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  val expandInstruction = new ShapeProperty[ExpandInstruction]

  override def transformProgram(program: Node, compilation: Compilation): Unit = {

    val classFile: ClassFile[NodePath] = PathRoot(program)
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
