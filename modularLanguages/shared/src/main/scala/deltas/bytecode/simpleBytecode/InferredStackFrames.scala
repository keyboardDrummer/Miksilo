package deltas.bytecode.simpleBytecode

import core.bigrammar.GrammarReference
import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node.Node
import core.language.{Compilation, Language}
import deltas.bytecode.ByteCodeSkeleton.ClassFile
import deltas.bytecode.attributes.StackMapTableAttributeDelta
import deltas.bytecode.attributes.StackMapTableAttributeDelta.{FullFrameLocals, FullFrameStack}
import deltas.bytecode.types.TypeSkeleton

object InferredStackFrames extends DeltaWithPhase with DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(LabelledLocations)

  def label(name: String): Node = LabelDelta.shape.create(LabelDelta.Name -> name)

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val classFile: ClassFile[Node] = program
    for (method <- classFile.methods) {
      val instructions = method.codeAttribute.instructions

      val stackLayouts = new InstructionTypeAnalysisForMethod(program, compilation, method)
      var previousStack = stackLayouts.initialStack
      var previousLocals = stackLayouts.parameters
      for ((label, index) <- instructions.zipWithIndex.filter(i => i._1.shape == LabelDelta.shape)) {
        val typeState = stackLayouts.typeStatePerInstruction(index)
        val currentStack = typeState.stackTypes
        val locals = getLocalTypesSequenceFromMap(typeState.variableTypes)
        label(LabelDelta.StackFrame) = getStackMap(previousStack, currentStack, previousLocals, locals)
        previousStack = currentStack
        previousLocals = locals
      }
    }

    def getLocalTypesSequenceFromMap(localTypes: Map[Int, Node]): Seq[Node] = {
      val max = (localTypes.keys ++ Seq(-1)).max
      0.to(max).map(index => localTypes.getOrElse(index, throw new NotImplementedError))
    }

    def toStackType(_type: Node): Node = TypeSkeleton.toStackType(_type, compilation)

    def getStackMap(previousStack: Seq[Node], stack: Seq[Node], previousLocals: Seq[Node], locals: Seq[Node]) = {
      getStackMapHelper(previousStack.map(toStackType), stack.map(toStackType), previousLocals.map(toStackType), locals.map(toStackType))
    }

    def getStackMapHelper(previousStack: Seq[Node], stack: Seq[Node], previousLocals: Seq[Node], locals: Seq[Node]) = {
      val sameLocalsPrefix = previousLocals.zip(locals).filter(p => p._1 == p._2)
      val removedLocals = previousLocals.drop(sameLocalsPrefix.length)
      val addedLocals = locals.drop(sameLocalsPrefix.length)
      val unchangedLocals = removedLocals.isEmpty && addedLocals.isEmpty
      if (unchangedLocals && stack.isEmpty) {
        new Node(StackMapTableAttributeDelta.SameFrameKey)
      }
      else if (unchangedLocals && stack.size == 1) {
        new Node(StackMapTableAttributeDelta.SameLocals1StackItem,
          StackMapTableAttributeDelta.SameLocals1StackItemType -> stack.head)
      }
      else if (stack.isEmpty && addedLocals.isEmpty) {
        new Node(StackMapTableAttributeDelta.ChopFrame, StackMapTableAttributeDelta.ChopFrameCount -> removedLocals.length)
      }
      else if (stack.isEmpty && removedLocals.isEmpty) {
        new Node(StackMapTableAttributeDelta.AppendFrame, StackMapTableAttributeDelta.AppendFrameTypes ->
          addedLocals.map(l => toStackType(l)))
      }
      else {
        new Node(StackMapTableAttributeDelta.FullFrame, FullFrameLocals -> locals, FullFrameStack -> stack)
      }
    }
  }

  override def description: String = "Generates a stack frame for each label instruction. " +
    "Stack frames can be used to determine the stack and variable types at a particular instruction."

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    find(LabelDelta.Shape).
      findAs(LabelDelta.StackFrame).
      asInstanceOf[GrammarReference].removeMe()
  }
}
