package transformations.bytecode

import core.transformation.{Contract, MetaObject, ProgramTransformation, TransformationState}
import transformations.bytecode.ByteCodeSkeleton.{FullFrameLocals, FullFrameStack}
import transformations.javac.base.ConstantPool

object InferredStackFrames extends ProgramTransformation {
  override def dependencies: Set[Contract] = Set(LabelledJumps)

  def label(name: String) = new MetaObject(LabelledJumps.LabelKey) {
    data.put(LabelledJumps.LabelNameKey, name)
  }

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    val signatureRegistry = ByteCodeSkeleton.getInstructionSignatureRegistry(state)
    val clazz = program
    val constantPool = new ConstantPool(ByteCodeSkeleton.getConstantPool(clazz))
    for (method <- ByteCodeSkeleton.getMethods(clazz)) {
      val methodDescriptor = constantPool.getValue(ByteCodeSkeleton.getMethodDescriptorIndex(method)).asInstanceOf[MetaObject]
      val inputs = ByteCodeSkeleton.getMethodDescriptorParameters(methodDescriptor)
      val code = ByteCodeSkeleton.getMethodAttributes(method).find(a => a.clazz == ByteCodeSkeleton.CodeKey).get
      val instructions = ByteCodeSkeleton.getCodeInstructions(code)
      val codeLocals = inputs
      var previousStack = Seq[MetaObject]()
      val stackAnalysis: StackAnalysis = new StackAnalysis(instructions,
        instruction => signatureRegistry(instruction.clazz)(constantPool, instruction)._1,
        instruction => signatureRegistry(instruction.clazz)(constantPool, instruction)._2)
      val currentStacks = stackAnalysis.run(0, previousStack)
      var previousLocals = codeLocals
      for (indexedLabel <- instructions.zipWithIndex.filter(i => i._1.clazz == LabelledJumps.LabelKey)) {
        val index = indexedLabel._2
        val label = indexedLabel._1
        val stackSize = currentStacks(index)
        val locals = codeLocals
        label(LabelledJumps.LabelStackFrame) = getStackMap(previousStack, stackSize, previousLocals, locals)
        previousStack = stackSize
        previousLocals = locals
      }
    }
  }

  def getStackMap(previousStack: Seq[Any], stack: Seq[Any], previousLocals: Seq[Any], locals: Seq[Any]) = {
    val sameLocalsPrefix = previousLocals.zip(locals).filter(p => p._1 == p._2)
    val removedLocals = previousLocals.drop(sameLocalsPrefix.length)
    val addedLocals = locals.drop(sameLocalsPrefix.length)
    val unchangedLocals = removedLocals.isEmpty && addedLocals.isEmpty
    if (unchangedLocals && stack.isEmpty) {
      new MetaObject(ByteCodeSkeleton.SameFrameKey)
    }
    else if (unchangedLocals && stack.size == 1) {
      new MetaObject(ByteCodeSkeleton.SameLocals1StackItem) {
        data.put(ByteCodeSkeleton.SameLocals1StackItemType, stack(0))
      }
    }
    else if (stack.isEmpty && addedLocals.isEmpty) {
      new MetaObject(ByteCodeSkeleton.ChopFrame) {
        data.put(ByteCodeSkeleton.ChopFrameCount, removedLocals.length)
      }
    }
    else if (stack.isEmpty && removedLocals.isEmpty) {
      new MetaObject(ByteCodeSkeleton.AppendFrame) {
        data.put(ByteCodeSkeleton.AppendFrameTypes, addedLocals)
      }
    }
    else {
      new MetaObject(ByteCodeSkeleton.FullFrame, FullFrameLocals -> locals, FullFrameStack -> stack)
    }

  }


}
