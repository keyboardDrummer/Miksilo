package transformations.bytecode.simpleBytecode

import core.bigrammar.{GrammarSelection, Labelled}
import core.particles.grammars.{GrammarCatalogue, KeyGrammar, ProgramGrammar}
import core.particles.node.Node
import core.particles.{CompilationState, Contract, ParticleWithGrammar, ParticleWithPhase}
import transformations.bytecode.additions.LabelledLocations
import transformations.bytecode.additions.LabelledLocations.LabelStackFrame
import transformations.bytecode.attributes.StackMapTableAttribute.{FullFrameLocals, FullFrameStack}
import transformations.bytecode.attributes.{CodeAttribute, StackMapTableAttribute}
import transformations.bytecode.types.TypeSkeleton
import transformations.bytecode.{ByteCodeMethodInfo, ByteCodeSkeleton}

object InferredStackFrames extends ParticleWithPhase with ParticleWithGrammar {

  override def dependencies: Set[Contract] = Set(LabelledLocations)

  def label(name: String) = new Node(LabelledLocations.LabelKey, LabelledLocations.LabelNameKey -> name)

  override def transform(program: Node, state: CompilationState): Unit = {
    val clazz = program
    for (method <- ByteCodeSkeleton.getMethods(clazz)) {
      val codeAnnotation = ByteCodeMethodInfo.getMethodAttributes(method).find(a => a.clazz == CodeAttribute.CodeKey).get
      val instructions = CodeAttribute.getCodeInstructions(codeAnnotation)

      val stackLayouts = new InstructionTypeAnalysisFromState(state, method)
      var previousStack = stackLayouts.initialStack
      var previousLocals = stackLayouts.parameters
      for (indexedLabel <- instructions.zipWithIndex.filter(i => i._1.clazz == LabelledLocations.LabelKey)) {
        val index = indexedLabel._2
        val label = indexedLabel._1
        val currentStack = stackLayouts.typeStatePerInstruction(index).stackTypes
        val localTypes = stackLayouts.typeStatePerInstruction(index).variableTypes
        val locals = getLocalTypesSequenceFromMap(localTypes)
        label(LabelledLocations.LabelStackFrame) = getStackMap(previousStack, currentStack, previousLocals, locals)
        previousStack = currentStack
        previousLocals = locals
      }
    }

    def getLocalTypesSequenceFromMap(localTypes: Map[Int, Node]): Seq[Node] = {
      val max = (localTypes.keys ++ Seq(-1)).max
      0.to(max).map(index => localTypes.getOrElse(index, throw new NotImplementedError))
    }

    def toStackType(_type: Node) = TypeSkeleton.toStackType(_type, state)

    def getStackMap(previousStack: Seq[Node], stack: Seq[Node], previousLocals: Seq[Node], locals: Seq[Node]) = {
      getStackMapHelper(previousStack.map(toStackType), stack.map(toStackType), previousLocals.map(toStackType), locals.map(toStackType))
    }

    def getStackMapHelper(previousStack: Seq[Node], stack: Seq[Node], previousLocals: Seq[Node], locals: Seq[Node]) = {
      val sameLocalsPrefix = previousLocals.zip(locals).filter(p => p._1 == p._2)
      val removedLocals = previousLocals.drop(sameLocalsPrefix.length)
      val addedLocals = locals.drop(sameLocalsPrefix.length)
      val unchangedLocals = removedLocals.isEmpty && addedLocals.isEmpty
      if (unchangedLocals && stack.isEmpty) {
        new Node(StackMapTableAttribute.SameFrameKey)
      }
      else if (unchangedLocals && stack.size == 1) {
        new Node(StackMapTableAttribute.SameLocals1StackItem, StackMapTableAttribute.SameLocals1StackItemType -> stack.head)
      }
      else if (stack.isEmpty && addedLocals.isEmpty) {
        new Node(StackMapTableAttribute.ChopFrame, StackMapTableAttribute.ChopFrameCount -> removedLocals.length)
      }
      else if (stack.isEmpty && removedLocals.isEmpty) {
        new Node(StackMapTableAttribute.AppendFrame, StackMapTableAttribute.AppendFrameTypes -> addedLocals.map(toStackType))
      }
      else {
        new Node(StackMapTableAttribute.FullFrame, FullFrameLocals -> locals, FullFrameStack -> stack)
      }
    }
  }

  override def description: String = "Generates a stack frame for each label instruction. " +
    "Stack frames can be used to determine the stack and variable types at a particular instruction."

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val labelMapPath = grammars.findPathsToKey(ProgramGrammar, KeyGrammar(LabelledLocations.LabelKey)).head
    val labelLabel: Labelled = labelMapPath.get.asInstanceOf[Labelled]
    val labelMap = labelLabel.inner.asInstanceOf[NodeMap]
    val newFields = labelMap.fields.filter(field => field != LabelStackFrame)
    labelLabel.inner = new NodeMap(labelMap.inner, labelMap.key, newFields)

    val stackMapTablePath = grammars.findPathsToKey(KeyGrammar(LabelledLocations.LabelKey), StackMapTableAttribute.StackMapFrameGrammar).head
    stackMapTablePath.previous.asInstanceOf[GrammarSelection].previous.asInstanceOf[GrammarSelection].removeMeFromSequence()
  }
}
