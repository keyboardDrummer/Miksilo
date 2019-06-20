package deltas.bytecode.simpleBytecode

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.language.{Compilation, Language}
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.attributes.CodeAttributeDelta._
import deltas.bytecode.attributes.StackMapTableAttributeDelta.{StackMapFrameGrammar, offsetGrammarKey}
import deltas.bytecode.attributes.{AttributeNameKey, CodeAttributeDelta, StackMapTableAttributeDelta}
import deltas.bytecode.coreInstructions.GotoDelta
import deltas.bytecode.coreInstructions.InstructionInstance.Instruction
import deltas.bytecode.coreInstructions.integers.integerCompare._
import deltas.bytecode.simpleBytecode.LabelDelta.Label

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object LabelledLocations extends DeltaWithPhase with DeltaWithGrammar {

  def jump(delta: JumpInstruction, target: String): Node = delta.shape.create(JumpName -> target)
  def ifZero(target: String): Node = jump(IfZeroDelta, target)
  def ifNotZero(target: String): Node = jump(IfNotZero, target)
  def goTo(target: String): Node = jump(GotoDelta, target)
  def ifIntegerCompareGreaterEquals(target: String): Node = jump(IfIntegerCompareGreaterOrEqualDelta, target)
  def ifIntegerCompareLess(target: String): Node = jump(IfIntegerCompareLessDelta, target)
  def ifIntegerCompareGreater(target: String): Node = jump(IfIntegerCompareGreaterDelta, target)
  def ifIntegerCompareEquals(target: String): Node = jump(IfIntegerCompareEqualDelta, target)
  def ifIntegerCompareNotEquals(target: String): Node = jump(IfIntegerCompareNotEqualDelta, target)
  def ifIntegerCompareLessEquals(target: String): Node = jump(IfIntegerCompareLessOrEqualDelta, target)

  override def inject(language: Language): Unit = {
    super.inject(language)
    LabelDelta.inject(language)
  }

  def transformProgram(program: Node, compilation: Compilation): Unit = {

    val instructionDeltas = CodeAttributeDelta.instructions.get(compilation)
    def instructionSize(instruction: Node) = instructionDeltas(instruction.shape).getInstructionSize(compilation)

    val classFile = program
    val codeAnnotations = CodeAttributeDelta.getCodeAnnotations(classFile)

    for (codeAnnotation <- codeAnnotations) {
      processCodeAnnotation(codeAnnotation)
    }

    def processCodeAnnotation(codeAnnotation: CodeAttribute[Node]): Unit = {
      val instructions = codeAnnotation.instructions
      val labelLocations: Map[String, Int] = determineLabelLocations(instructions)
      codeAnnotation.attributes = codeAnnotation.attributes ++ getStackMapTable(labelLocations, instructions)
      codeAnnotation.instructions = getNewInstructions(instructions, labelLocations)
    }

    def determineLabelLocations(instructions: Seq[Node]): Map[String, Int] = {
      val targetLocations = mutable.Map[String, Int]()
      var location = 0
      for (instruction <- instructions) {
        if (instruction.shape == LabelDelta.shape) {
          targetLocations(new Label(instruction).name) = location
        }

        location += instructionSize(instruction)
      }
      targetLocations.toMap
    }

    def getNewInstructions(instructions: Seq[Instruction[Node]], targetLocations: Map[String, Int]): ArrayBuffer[Node] = {
      var newInstructions = mutable.ArrayBuffer[Node]()
      newInstructions.sizeHint(instructions.length)

      var location = 0
      for (instruction <- instructions) {

        if (instruction.jumpBehavior.hasJumpInFirstArgument) {
          setInstructionArguments(instruction, Seq(targetLocations(getJumpInstructionLabel(instruction)) - location))
          instruction.data.remove(JumpName)
        }

        if (instruction.shape != LabelDelta.shape)
          newInstructions += instruction

        location += instructionSize(instruction)
      }

      newInstructions
    }
  }

  def getJumpInstructionLabel(instruction: Node): String = {
    instruction(JumpName).asInstanceOf[String]
  }

  def getStackMapTable(labelLocations: Map[String, Int], instructions: Seq[Node]): Seq[Node] = {
    val locationsWithFrame = instructions.filter(i => i.shape == LabelDelta.Shape).map(i => new Label(i)).
      map(i => (labelLocations(i.name), i.stackFrame))
    var locationAfterPreviousFrame = 0
    var stackFrames = ArrayBuffer[Node]()
    stackFrames.sizeHint(locationsWithFrame.size)
    for ((location, frame) <- locationsWithFrame) {
      val offset = location - locationAfterPreviousFrame
      if (offset >= 0) { //TODO add a test-case for consecutive labels.
        frame(StackMapTableAttributeDelta.FrameOffset) = offset
        stackFrames += frame
        locationAfterPreviousFrame = location + 1
      }
    }
    if (stackFrames.nonEmpty) {
      Seq(StackMapTableAttributeDelta.Shape.create(
        AttributeNameKey -> StackMapTableAttributeDelta.entry,
        StackMapTableAttributeDelta.Maps -> stackFrames))
    }
    else
      Seq.empty[Node]
  }

  override def dependencies: Set[Contract] = Set(StackMapTableAttributeDelta, ByteCodeSkeleton, IfIntegerCompareGreaterOrEqualDelta, GotoDelta, IfZeroDelta)

  override def description: String = "Replaces the jump instructions from bytecode. " +
    "The new instructions are similar to the old ones except that they use labels as target instead of instruction indices."

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    replaceJumpIndicesWithLabels(grammars, language)
    removeOffsetFromStackMapFrameGrammars(grammars)
    import grammars._
    find(ByteCodeSkeleton.AttributeGrammar).findLabelled(StackMapTableAttributeDelta.Shape).removeMeFromOption()
  }

  def removeOffsetFromStackMapFrameGrammars(grammars: LanguageGrammars): Unit = {
    val offsetGrammar = grammars.find(offsetGrammarKey)
    import grammars._
    val offsetGrammarPaths = find(StackMapFrameGrammar).descendants.filter(path => path.value == offsetGrammar)
    offsetGrammarPaths.foreach(delta => delta.removeMe())
  }

  object JumpName extends NodeField
  def replaceJumpIndicesWithLabels(_grammars: LanguageGrammars, language: Language): Unit = {
    val grammars = _grammars
    import grammars._
    val instructionDeltas = CodeAttributeDelta.instructions.get(language).values
    val jumpInstructionDeltas = instructionDeltas.filter(v => v.jumpBehavior.hasJumpInFirstArgument)
    for(jump <- jumpInstructionDeltas)
    {
      val grammar = find(jump.shape)
      grammar.inner = jump.grammarName ~~> LabelDelta.getNameGrammar(grammars).as(JumpName) asNode jump.shape
    }
  }
}
