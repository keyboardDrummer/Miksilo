package languages.bytecode

import transformation.{TransformationState, MetaObject, ProgramTransformation}
import scala.collection.mutable
import languages.javac.base.ConstantPool

object NoStackFrame extends ProgramTransformation {
  override def dependencies: Set[ProgramTransformation] = Set(ByteCodeGoTo)

  def getStackMap(previousStack: Seq[Any], stack: Seq[Any], previousLocals: Seq[Any], locals: Seq[Any]) = {
    val sameLocalsPrefix = previousLocals.zip(locals).filter(p => p._1 == p._2)
    val removedLocals = previousLocals.drop(sameLocalsPrefix.length)
    val addedLocals = locals.drop(sameLocalsPrefix.length)
    val unchangedLocals = removedLocals.isEmpty && addedLocals.isEmpty
    if (unchangedLocals && stack.isEmpty) {
      new MetaObject(ByteCode.SameFrameKey)
    }
    else if (unchangedLocals && stack.size == 1) {
      new MetaObject(ByteCode.SameLocals1StackItem) {
        data.put(ByteCode.SameLocals1StackItemType, stack(0))
      }
    }
    else if (stack.isEmpty && addedLocals.isEmpty) {
      new MetaObject(ByteCode.ChopFrame) {
        data.put(ByteCode.ChopFrameCount, removedLocals.length)
      }
    }
    else if (stack.isEmpty && removedLocals.isEmpty) {
      new MetaObject(ByteCode.AppendFrame) {
        data.put(ByteCode.AppendFrameTypes, addedLocals)
      }
    }
    else {
      new MetaObject(ByteCode.FullFrame) {
        data.put(ByteCode.FullFrameLocals, locals)
        data.put(ByteCode.FullFrameStack, stack)
      }
    }

  }

  def label(name: String) = new MetaObject(ByteCodeGoTo.LabelKey) {
    data.put(ByteCodeGoTo.LabelNameKey, name)
  }

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    val clazz = program
    val constantPool = new ConstantPool(ByteCode.getConstantPool(clazz))
    for (method <- ByteCode.getMethods(clazz)) {
      val methodDescriptor = constantPool.getValue(ByteCode.getMethodDescriptorIndex(method)).asInstanceOf[MetaObject]
      val inputs = ByteCode.getMethodDescriptorParameters(methodDescriptor)
      val code = ByteCode.getMethodAttributes(method).find(a => a.clazz == ByteCode.CodeKey).get
      val instructions = ByteCode.getCodeInstructions(code)
      val codeLocals = inputs
      var previousStack = Seq[Any]()
      val currentStacks = new StackAnalysis(constantPool, instructions).run(0, previousStack)
      var previousLocals = codeLocals
      for (indexedLabel <- instructions.zipWithIndex.filter(i => i._1.clazz == ByteCodeGoTo.LabelKey)) {
        val index = indexedLabel._2
        val label = indexedLabel._1
        val stackSize = currentStacks(index)
        val locals = codeLocals
        label(ByteCodeGoTo.LabelStackFrame) = getStackMap(previousStack, stackSize, previousLocals, locals)
        previousStack = stackSize
        previousLocals = locals
      }
    }
  }


}
