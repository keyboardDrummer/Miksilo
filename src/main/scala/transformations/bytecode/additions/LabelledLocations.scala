package transformations.bytecode.additions

import core.bigrammar.{BiGrammar, StringLiteral}
import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.attributes.CodeAttribute._
import transformations.bytecode.attributes.StackMapTableAttribute.{StackMapFrameGrammar, offsetGrammarKey}
import transformations.bytecode.attributes.{AttributeNameKey, CodeAttribute, InstructionArgumentsKey, StackMapTableAttribute}
import transformations.bytecode.coreInstructions.integers.integerCompare._
import transformations.bytecode.coreInstructions.{GotoDelta, InstructionDelta, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object LabelledLocations extends DeltaWithPhase with DeltaWithGrammar {
  def ifZero(target: String) = instruction(IfZeroDelta.Clazz, Seq(target))
  def ifNotZero(target: String) = instruction(IfNotZero.key, Seq(target))

  def goTo(target: String) = instruction(GotoDelta.GoToKey, Seq(target))

  def ifIntegerCompareGreaterEquals(target: String) = instruction(IfIntegerCompareGreaterOrEqualDelta.Clazz, Seq(target))
  def ifIntegerCompareLess(target: String) = instruction(IfIntegerCompareLessDelta.key, Seq(target))
  def ifIntegerCompareGreater(target: String) = instruction(IfIntegerCompareGreaterDelta.key, Seq(target))
  def ifIntegerCompareEquals(target: String) = instruction(IfIntegerCompareEqualDelta.key, Seq(target))
  def ifIntegerCompareNotEquals(target: String) = instruction(IfIntegerCompareNotEqualDelta.key, Seq(target))
  def ifIntegerCompareLessEquals(target: String) = instruction(IfIntegerCompareLessOrEqualDelta.key, Seq(target))

  def label(name: String, stackFrame: Node) = new Node(LabelKey,
    LabelName -> name,
    LabelStackFrame -> stackFrame)

  object GeneratedLabels extends NodeField
  def getUniqueLabel(suggestion: String, methodInfo: Node, state: Language): String = {
    val taken: mutable.Set[String] = methodInfo.data.getOrElseUpdate(GeneratedLabels, mutable.Set.empty).
      asInstanceOf[mutable.Set[String]]
    var result = suggestion
    var increment = 0
    while(taken.contains(result))
    {
      increment += 1
      result = suggestion + "_" + increment
    }
    taken.add(result)
    "<" + result + ">"
  }

  override def inject(state: Language): Unit = {
    super.inject(state)
    LabelDelta.inject(state)
  }

  def transform(program: Node, state: Compilation): Unit = {

    val jumpRegistry = CodeAttribute.getState(state.language).jumpBehaviorRegistry
    def instructionSize(instruction: Node) = CodeAttribute.getInstructionSizeRegistry(state.language)(instruction.clazz)

    def getNewInstructions(instructions: Seq[Node], targetLocations: Map[String, Int]): ArrayBuffer[Node] = {
      var newInstructions = mutable.ArrayBuffer[Node]()
      newInstructions.sizeHint(instructions.length)

      var location = 0
      for (instruction <- instructions) {

        if (jumpRegistry(instruction.clazz).hasJumpInFirstArgument) {
          setInstructionArguments(instruction, Seq(targetLocations(getJumpInstructionLabel(instruction)) - location))
        }

        if (instruction.clazz != LabelKey)
          newInstructions += instruction

        location += instructionSize(instruction)
      }

      newInstructions
    }

    val clazz = program
    val codeAnnotations = CodeAttribute.getCodeAnnotations(clazz)

    for (codeAnnotation <- codeAnnotations) {
      processCodeAnnotation(codeAnnotation)
    }

    def processCodeAnnotation(codeAnnotation: CodeWrapper[Node]): Unit = {
      val instructions = codeAnnotation.instructions
      val targetLocations: Map[String, Int] = determineTargetLocations(instructions)
      codeAnnotation(CodeAttribute.CodeAttributesKey) = CodeAttribute.getCodeAttributes(codeAnnotation) ++
        getStackMapTable(targetLocations, instructions)

      val newInstructions: Seq[Node] = getNewInstructions(instructions, targetLocations)
      codeAnnotation(CodeAttribute.Instructions) = newInstructions
    }

    def determineTargetLocations(instructions: Seq[Node]): Map[String, Int] = {
      val targetLocations = mutable.Map[String, Int]()
      var location = 0
      for (instruction <- instructions) {
        instruction.clazz match {
          case LabelKey => targetLocations(getLabelName(instruction)) = location
          case _ =>
        }

        location += instructionSize(instruction)
      }
      targetLocations.toMap
    }
  }

  def getJumpInstructionLabel(instruction: Node): String = {
    getInstructionArguments(instruction).head.asInstanceOf[String]
  }

  def getStackMapTable(labelLocations: Map[String, Int], instructions: Seq[Node]): Seq[Node] = {
    val framesPerLocation = instructions.filter(i => i.clazz == LabelKey).
      map(i => (labelLocations(getLabelName(i)), getLabelStackFrame(i))).toMap
    var locationAfterPreviousFrame = 0
    var stackFrames = ArrayBuffer[Node]()
    stackFrames.sizeHint(framesPerLocation.size)
    for (location <- framesPerLocation.keys.toSeq.sorted) {
      val frame = framesPerLocation(location)
      val offset = location - locationAfterPreviousFrame

      frame(StackMapTableAttribute.FrameOffset) = offset
      stackFrames += frame
      locationAfterPreviousFrame = location + 1
    }
    if (stackFrames.nonEmpty) {
      Seq(StackMapTableAttribute.Clazz.create(
        AttributeNameKey -> StackMapTableAttribute.entry,
        StackMapTableAttribute.Maps -> stackFrames))
    }
    else
      Seq.empty[Node]
  }

  def getLabelStackFrame(label: Node) = label(LabelStackFrame).asInstanceOf[Node]

  def getLabelName(label: Node) = label(LabelName).asInstanceOf[String]

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton, IfIntegerCompareGreaterOrEqualDelta, GotoDelta, IfZeroDelta)

  object LabelDelta extends InstructionDelta {
    override val key = LabelKey

    override def getInstructionByteCode(instruction: Node): Seq[Byte] = throw new UnsupportedOperationException()

    override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature = {
      InstructionSignature(Seq.empty, Seq.empty)
    }

    override def getInstructionSize: Int = 0

    override def getGrammarForThisInstruction(grammars: GrammarCatalogue): BiGrammar = {
      val stackMapFrameGrammar = grammars.find(StackMapFrameGrammar)
      grammarName ~~> StringLiteral.as(LabelName) %
        stackMapFrameGrammar.indent().as(LabelStackFrame) asNode LabelKey
    }

    override def description: String = "Used to mark a specific point in an instruction list."

    override def grammarName: String = "label"
  }

  object LabelKey extends NodeClass

  object LabelName extends NodeField

  object LabelStackFrame extends NodeField

  override def description: String = "Replaces the jump instructions from bytecode. " +
    "The new instructions are similar to the old ones except that they use labels as target instead of instruction indices."

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    overrideJumpGrammars(grammars)
    overrideStackMapFrameGrammars(grammars)
  }

  def overrideStackMapFrameGrammars(grammars: GrammarCatalogue): Unit = {
    val offsetGrammar = grammars.find(offsetGrammarKey)
    val offsetGrammarPaths = grammars.find(StackMapFrameGrammar).descendants.filter(path => path.get == offsetGrammar)
    offsetGrammarPaths.foreach(delta => delta.removeMeFromSequence())
  }

  def overrideJumpGrammars(grammars: GrammarCatalogue): Unit = {
    val jumps = Seq[InstructionDelta](IfZeroDelta, IfNotZero, GotoDelta,
      IfIntegerCompareGreaterOrEqualDelta,
      IfIntegerCompareLessDelta, IfIntegerCompareEqualDelta, IfIntegerCompareNotEqualDelta)
    for(jump <- jumps)
    {
      val grammar = grammars.find(jump.key)
      grammar.inner = jump.grammarName ~~> StringLiteral.manySeparated(" ").as(InstructionArgumentsKey) asNode jump.key
    }
  }
}
