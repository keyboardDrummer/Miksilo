package transformations.bytecode.simpleBytecode

import core.transformation.sillyCodePieces.ParticleWithPhase
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.{ByteCodeMethodInfo, ByteCodeSkeleton}
import transformations.bytecode.additions.LabelledTargets
import transformations.bytecode.attributes.StackMapTableAttribute.{FullFrameLocals, FullFrameStack}
import transformations.bytecode.attributes.{CodeAttribute, StackMapTableAttribute}
import transformations.types.TypeSkeleton

object InferredStackFrames extends ParticleWithPhase {

  override def dependencies: Set[Contract] = Set(LabelledTargets)

  def label(name: String) = new MetaObject(LabelledTargets.LabelKey) {
    data.put(LabelledTargets.LabelNameKey, name)
  }

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    val clazz = program
    val constantPool = ByteCodeSkeleton.getConstantPool(clazz)
    for (method <- ByteCodeSkeleton.getMethods(clazz)) {
      val codeAnnotation = ByteCodeMethodInfo.getMethodAttributes(method).find(a => a.clazz == CodeAttribute.CodeKey).get
      val instructions = CodeAttribute.getCodeInstructions(codeAnnotation)

      val stackLayouts = new InstructionTypeAnalysisFromState(state, method)
      var previousStack = stackLayouts.initialStack
      var previousLocals = stackLayouts.parameters
      for (indexedLabel <- instructions.zipWithIndex.filter(i => i._1.clazz == LabelledTargets.LabelKey)) {
        val index = indexedLabel._2
        val label = indexedLabel._1
        val currentStack = stackLayouts.typeStatePerInstruction(index).stackTypes
        val localTypes = stackLayouts.typeStatePerInstruction(index).variableTypes
        val locals = getLocalTypesSequenceFromMap(localTypes)
        label(LabelledTargets.LabelStackFrame) = getStackMap(previousStack, currentStack, previousLocals, locals)
        previousStack = currentStack
        previousLocals = locals
      }
    }

    def getLocalTypesSequenceFromMap(localTypes: Map[Int, MetaObject]): Seq[MetaObject] = {
      val max = localTypes.keys.max
      0.to(max).map(index => localTypes.getOrElse(index, throw new NotImplementedError))
    }

    def toStackType(_type: MetaObject) = TypeSkeleton.toStackType(constantPool, _type)

    def getStackMap(previousStack: Seq[MetaObject], stack: Seq[MetaObject], previousLocals: Seq[MetaObject], locals: Seq[MetaObject]) = {
      getStackMapHelper(previousStack.map(toStackType), stack.map(toStackType), previousLocals.map(toStackType), locals.map(toStackType))
    }

    def getStackMapHelper(previousStack: Seq[MetaObject], stack: Seq[MetaObject], previousLocals: Seq[MetaObject], locals: Seq[MetaObject]) = {
      val sameLocalsPrefix = previousLocals.zip(locals).filter(p => p._1 == p._2)
      val removedLocals = previousLocals.drop(sameLocalsPrefix.length)
      val addedLocals = locals.drop(sameLocalsPrefix.length)
      val unchangedLocals = removedLocals.isEmpty && addedLocals.isEmpty
      if (unchangedLocals && stack.isEmpty) {
        new MetaObject(StackMapTableAttribute.SameFrameKey)
      }
      else if (unchangedLocals && stack.size == 1) {
        new MetaObject(StackMapTableAttribute.SameLocals1StackItem) {
          data.put(StackMapTableAttribute.SameLocals1StackItemType, stack(0))
        }
      }
      else if (stack.isEmpty && addedLocals.isEmpty) {
        new MetaObject(StackMapTableAttribute.ChopFrame) {
          data.put(StackMapTableAttribute.ChopFrameCount, removedLocals.length)
        }
      }
      else if (stack.isEmpty && removedLocals.isEmpty) {
        new MetaObject(StackMapTableAttribute.AppendFrame) {
          data.put(StackMapTableAttribute.AppendFrameTypes, addedLocals.map(toStackType))
        }
      }
      else {
        new MetaObject(StackMapTableAttribute.FullFrame, FullFrameLocals -> locals, FullFrameStack -> stack)
      }

    }
  }

  override def description: String = "Generates a stack frame for each label instruction. " +
    "Stack frames can be used to determine the stack and variable types at a particular instruction."
}
