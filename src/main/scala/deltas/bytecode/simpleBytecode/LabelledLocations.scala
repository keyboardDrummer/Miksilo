package deltas.bytecode.simpleBytecode

import core.bigrammar.grammars.StringLiteral
import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.attributes.CodeAttribute._
import deltas.bytecode.attributes.StackMapTableAttribute.{StackMapFrameGrammar, offsetGrammarKey}
import deltas.bytecode.attributes.{AttributeNameKey, CodeAttribute, StackMapTableAttribute}
import deltas.bytecode.coreInstructions.integers.integerCompare._
import deltas.bytecode.coreInstructions.{GotoDelta, InstructionDelta}
import deltas.bytecode.simpleBytecode.LabelDelta.Label

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object LabelledLocations extends DeltaWithPhase with DeltaWithGrammar {

  def jump(key: NodeClass, target: String) = key.create(JumpName -> target)
  def ifZero(target: String): Node = jump(IfZeroDelta.Clazz, target)
  def ifNotZero(target: String): Node = jump(IfNotZero.key, target)
  def goTo(target: String): Node = jump(GotoDelta.GoToKey, target)
  def ifIntegerCompareGreaterEquals(target: String): Node = jump(IfIntegerCompareGreaterOrEqualDelta.Clazz, target)
  def ifIntegerCompareLess(target: String): Node = jump(IfIntegerCompareLessDelta.key, target)
  def ifIntegerCompareGreater(target: String): Node = jump(IfIntegerCompareGreaterDelta.key, target)
  def ifIntegerCompareEquals(target: String): Node = jump(IfIntegerCompareEqualDelta.key, target)
  def ifIntegerCompareNotEquals(target: String): Node = jump(IfIntegerCompareNotEqualDelta.key, target)
  def ifIntegerCompareLessEquals(target: String): Node = jump(IfIntegerCompareLessOrEqualDelta.key, target)

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

  def transformProgram(program: Node, compilation: Compilation): Unit = {

    val jumpRegistry = CodeAttribute.getRegistry(compilation.language).jumpBehaviorRegistry
    def instructionSize(instruction: Node) = CodeAttribute.getInstructionSizeRegistry(compilation.language)(instruction.clazz)

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

        if (jumpRegistry(instruction.clazz).hasJumpInFirstArgument) {
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

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    replaceJumpIndicesWithLabels(grammars)
    removeOffsetFromStackMapFrameGrammars(grammars)
    //Why not remove StackMapFrameAttribute?
  }

  def removeOffsetFromStackMapFrameGrammars(grammars: LanguageGrammars): Unit = {
    val offsetGrammar = grammars.find(offsetGrammarKey)
    val offsetGrammarPaths = grammars.find(StackMapFrameGrammar).descendants.filter(path => path.value == offsetGrammar)
    offsetGrammarPaths.foreach(delta => delta.removeMe())
  }

  object JumpName extends NodeField
  def replaceJumpIndicesWithLabels(grammars: LanguageGrammars): Unit = {
    import grammars._
    val jumps = Seq[InstructionDelta](IfZeroDelta, IfNotZero, GotoDelta,
      IfIntegerCompareGreaterOrEqualDelta,
      IfIntegerCompareLessDelta, IfIntegerCompareEqualDelta, IfIntegerCompareNotEqualDelta) //TODO replace hardcoded list with registry lookup
    for(jump <- jumps)
    {
      val grammar = find(jump.key)
      grammar.inner = jump.grammarName ~~> StringLiteral.as(JumpName) asNode jump.key
    }
  }
}
