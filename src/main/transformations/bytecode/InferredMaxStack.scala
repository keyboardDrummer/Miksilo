package transformations.bytecode

import core.transformation.{Contract, MetaObject, ProgramTransformation, TransformationState}

object InferredMaxStack extends ProgramTransformation {
  override def dependencies: Set[Contract] = Set(LabelledJumps)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    val clazz = program
    val constantPool = ByteCode.getConstantPool(clazz)
    for (method <- ByteCode.getMethods(clazz)) {
      val code = ByteCode.getMethodAttributes(method).find(a => a.clazz == ByteCode.CodeKey).get
      code(ByteCode.CodeMaxStackKey) = getMaxStack(constantPool, code)
    }
  }

  def getMaxStack(constantPool: Seq[Any], code: MetaObject): Integer = {
    val instructions = ByteCode.getCodeInstructions(code)
    val currentStacks = new StackSizeAnalysis(constantPool, instructions).run(0, 0)
    val maxStack = currentStacks.values.max
    maxStack
  }
}
