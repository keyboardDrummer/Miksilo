package deltas.bytecode.simpleBytecode

import core.bigrammar.grammars.StringLiteral
import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.attributes.CodeAttributeDelta._
import deltas.bytecode.attributes.StackMapTableAttribute.{StackMapFrameGrammar, offsetGrammarKey}
import deltas.bytecode.attributes.{AttributeNameKey, CodeAttributeDelta, StackMapTableAttribute}
import deltas.bytecode.coreInstructions.GotoDelta
import deltas.bytecode.coreInstructions.integers.integerCompare._
import deltas.bytecode.simpleBytecode.LabelDelta.Label

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object LabelledLocations extends DeltaWithPhase with DeltaWithGrammar {

  def jump(delta: JumpInstruction, target: String): Node = delta.key.create(JumpName -> target)
  def ifZero(target: String): Node = jump(IfZeroDelta, target)
  def ifNotZero(target: String): Node = jump(IfNotZero, target)
  def goTo(target: String): Node = jump(GotoDelta, target)
  def ifIntegerCompareGreaterEquals(target: String): Node = jump(IfIntegerCompareGreaterOrEqualDelta, target)
  def ifIntegerCompareLess(target: String): Node = jump(IfIntegerCompareLessDelta, target)
  def ifIntegerCompareGreater(target: String): Node = jump(IfIntegerCompareGreaterDelta, target)
  def ifIntegerCompareEquals(target: String): Node = jump(IfIntegerCompareEqualDelta, target)
  def ifIntegerCompareNotEquals(target: String): Node = jump(IfIntegerCompareNotEqualDelta, target)
  def ifIntegerCompareLessEquals(target: String): Node = jump(IfIntegerCompareLessOrEqualDelta, target)

  override def inject(state: Language): Unit = {
    super.inject(state)
    LabelDelta.inject(state)
  }

  def transformProgram(program: Node, compilation: Compilation): Unit = {

    val instructionDeltas = CodeAttributeDelta.getRegistry(compilation.language).instructions
    def instructionSize(instruction: Node) = instructionDeltas(instruction.clazz).getInstructionSize

    val clazz = program
    val codeAnnotations = CodeAttributeDelta.getCodeAnnotations(clazz)

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
        instruction.clazz match {
          case LabelDelta.LabelKey => targetLocations(new Label(instruction).name) = location
          case _ =>
        }

        location += instructionSize(instruction)
      }
      targetLocations.toMap
    }

    def getNewInstructions(instructions: Seq[Node], targetLocations: Map[String, Int]): ArrayBuffer[Node] = {
      var newInstructions = mutable.ArrayBuffer[Node]()
      newInstructions.sizeHint(instructions.length)

      var location = 0
      for (instruction <- instructions) {

        if (instructionDeltas(instruction.clazz).jumpBehavior.hasJumpInFirstArgument) {
          setInstructionArguments(instruction, Seq(targetLocations(getJumpInstructionLabel(instruction)) - location))
          instruction.data.remove(JumpName)
        }

        if (instruction.clazz != LabelDelta.LabelKey)
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
    val framesPerLocation = instructions.filter(i => i.clazz == LabelDelta.LabelKey).map(i => new Label(i)).
      map(i => (labelLocations(i.name), i.stackFrame)).toMap
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

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton, IfIntegerCompareGreaterOrEqualDelta, GotoDelta, IfZeroDelta)

  override def description: String = "Replaces the jump instructions from bytecode. " +
    "The new instructions are similar to the old ones except that they use labels as target instead of instruction indices."

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    replaceJumpIndicesWithLabels(grammars, language)
    removeOffsetFromStackMapFrameGrammars(grammars)
    grammars.find(ByteCodeSkeleton.AttributeGrammar).findLabelled(StackMapTableAttribute.Clazz).removeMeFromOption()
  }

  def removeOffsetFromStackMapFrameGrammars(grammars: LanguageGrammars): Unit = {
    val offsetGrammar = grammars.find(offsetGrammarKey)
    val offsetGrammarPaths = grammars.find(StackMapFrameGrammar).descendants.filter(path => path.value == offsetGrammar)
    offsetGrammarPaths.foreach(delta => delta.removeMe())
  }

  object JumpName extends NodeField
  def replaceJumpIndicesWithLabels(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val instructionDeltas = CodeAttributeDelta.getRegistry(language).instructions.values
    val jumpInstructionDeltas = instructionDeltas.filter(v => v.jumpBehavior.hasJumpInFirstArgument)
    for(jump <- jumpInstructionDeltas)
    {
      val grammar = find(jump.key)
      grammar.inner = jump.grammarName ~~> StringLiteral.as(JumpName) asNode jump.key
    }
  }
}
