package deltas.bytecode.simpleBytecode

import core.deltas.node.Node
import core.language.Language
import deltas.bytecode.ByteCodeMethodInfo.MethodInfo
import deltas.bytecode.ByteCodeSkeleton.ClassFile
import deltas.bytecode.constants.ClassInfoConstant
import deltas.bytecode.coreInstructions.InstructionDelta.Instruction
import deltas.bytecode.coreInstructions.InstructionSignature
import deltas.bytecode.extraConstants.QualifiedClassNameConstantDelta
import deltas.bytecode.simpleBytecode.InstructionTypeAnalysis.InstructionSideEffects
import deltas.bytecode.types.ObjectTypeDelta
import deltas.bytecode.{ByteCodeMethodInfo, ByteCodeSkeleton}
import deltas.javac.classes.skeleton.QualifiedClassName

class InstructionTypeAnalysisForMethod(program: Node, language: Language, method: MethodInfo[Node]) {
  private val typeAnalysis = getTypeAnalysis
  val parameters: Seq[Node] = getMethodParameters
  private val initialVariables = parameters.zipWithIndex.map(p => p._2 -> p._1).toMap
  val initialStack: Seq[Node] = Seq[Node]()
  val initialProgramTypeState: ProgramTypeState = ProgramTypeState(initialStack, initialVariables)
  val typeStatePerInstruction: Map[Int, ProgramTypeState] = typeAnalysis.run(0, initialProgramTypeState)

  private def getTypeAnalysis = {
    val codeAnnotation = method.codeAttribute
    val instructions = codeAnnotation.instructions

    new InstructionTypeAnalysis(instructions) {
      override def getSideEffects(typeState: ProgramTypeState, instruction: Instruction[Node]): InstructionSideEffects =
        instruction.delta.getVariableUpdates(instruction, typeState)

      override def getSignature(typeState: ProgramTypeState, instruction: Instruction[Node]): InstructionSignature =
        instruction.delta.getSignature(instruction, typeState, language)
    }
  }

  private def getMethodParameters = {
    val methodIsStatic: Boolean = method.accessFlags.contains(ByteCodeMethodInfo.StaticAccess)
    val methodParameters = method._type.parameterTypes
    if (methodIsStatic) {
      methodParameters
    }
    else {
      val classFile: ClassFile[Node] = program
      val classRef = classFile(ByteCodeSkeleton.ClassNameIndexKey).asInstanceOf[Node]
      val className = classRef(ClassInfoConstant.Name).asInstanceOf[Node]
      Seq(ObjectTypeDelta.objectType(className(QualifiedClassNameConstantDelta.Value).asInstanceOf[QualifiedClassName])) ++ methodParameters
    }
  }
}
