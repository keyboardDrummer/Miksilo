package transformations.bytecode

import core.transformation.{Contract, MetaObject, ProgramTransformation, TransformationState}
import transformations.bytecode.LabelledJumps.LabelKey
import transformations.javac.base.ConstantPool

object InferredMaxStack extends ProgramTransformation {
  override def dependencies: Set[Contract] = Set(LabelledJumps)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    val clazz = program
    val constantPool = new ConstantPool(ByteCodeSkeleton.getConstantPool(clazz))

    def getMaxStack(code: MetaObject): Integer = {
      val instructions = ByteCodeSkeleton.getCodeInstructions(code)
      val registry = ByteCodeSkeleton.getInstructionStackSizeModificationRegistry(state)
      val currentStacks = new StackSizeAnalysis(instructions, instruction => registry(instruction.clazz)(constantPool, instruction)).run(0, 0)
      val maxStack = currentStacks.values.max
      maxStack
    }

    for (method <- ByteCodeSkeleton.getMethods(clazz)) {
      val code = ByteCodeSkeleton.getMethodAttributes(method).find(a => a.clazz == ByteCodeSkeleton.CodeKey).get
      code(ByteCodeSkeleton.CodeMaxStackKey) = getMaxStack(code)
    }
  }


  def getInstructionStackSizeModification(constantPool: Seq[Any], instruction: MetaObject): Integer =
    instruction.clazz match {
      case LabelKey => 0
    }
}
