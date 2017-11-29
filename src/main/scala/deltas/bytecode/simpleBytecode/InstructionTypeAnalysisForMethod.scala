package deltas.bytecode.simpleBytecode

import core.deltas.Language
import core.deltas.node.{Node, NodeClass}
import deltas.bytecode.ByteCodeMethodInfo.MethodInfo
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.attributes.CodeAttributeDelta.JumpBehavior
import deltas.bytecode.constants.ClassInfoConstant
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

    val instructionDeltas = CodeAttributeDelta.getRegistry(language).instructions
    new InstructionTypeAnalysis(instructions) {
      override def getSideEffects(typeState: ProgramTypeState, instruction: Node): InstructionSideEffects =
        instructionDeltas(instruction.clazz).getVariableUpdates(instruction, typeState)

      override def getSignature(typeState: ProgramTypeState, instruction: Node): InstructionSignature =
        instructionDeltas(instruction.clazz).getSignature(instruction, typeState, language)

      override def getJumpBehavior(instructionClazz: NodeClass): JumpBehavior = instructionDeltas(instructionClazz).jumpBehavior
    }
  }

  private def getMethodParameters = {
    val methodIsStatic: Boolean = method.accessFlags.contains(ByteCodeMethodInfo.StaticAccess)
    val methodParameters = method._type.parameterTypes
    if (methodIsStatic) {
      methodParameters
    }
    else {
      val clazz = program
      val clazzRef = clazz(ByteCodeSkeleton.ClassNameIndexKey).asInstanceOf[Node]
      val className = clazzRef(ClassInfoConstant.Name).asInstanceOf[Node]
      Seq(ObjectTypeDelta.objectType(className(QualifiedClassNameConstantDelta.Value).asInstanceOf[QualifiedClassName])) ++ methodParameters
    }
  }
}
