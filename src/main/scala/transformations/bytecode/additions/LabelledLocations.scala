package transformations.bytecode.additions

import core.bigrammar.{MissingValue, Consume, BiGrammar}
import core.grammar.StringLiteral
import core.particles.grammars.{KeyGrammar, GrammarCatalogue}
import core.particles.node.{Key, Node}
import core.particles.{ParticleWithGrammar, CompilationState, Contract, ParticleWithPhase}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.attributes.StackMapTableAttribute.{StackMapFrameGrammar, OffsetDelta, DeltaGrammar, StackMapTableGrammar}
import transformations.bytecode.attributes.{InstructionArgumentsKey, CodeAttribute, StackMapTableAttribute}
import transformations.bytecode.coreInstructions.integers.integerCompare.IfNotZero.IfNotZeroKey
import transformations.bytecode.coreInstructions.integers.integerCompare._
import transformations.bytecode.coreInstructions.{GotoC, InstructionC, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool

import transformations.bytecode.ByteCodeSkeleton._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import CodeAttribute._

object LabelledLocations extends ParticleWithPhase with ParticleWithGrammar {
  def ifZero(target: String) = instruction(IfZeroC.IfZeroKey, Seq(target))
  def ifNotZero(target: String) = instruction(IfNotZeroKey, Seq(target))

  def goTo(target: String) = instruction(GotoC.GoToKey, Seq(target))

  def ifIntegerCompareGreaterEquals(target: String) = instruction(IfIntegerCompareGreaterOrEqualC.IfIntegerCompareGreaterKey, Seq(target))
  def ifIntegerCompareLess(target: String) = instruction(IfIntegerCompareLessC.key, Seq(target))
  def ifIntegerCompareEquals(target: String) = instruction(IfIntegerCompareEqualC.key, Seq(target))
  def ifIntegerCompareNotEquals(target: String) = instruction(IfIntegerCompareNotEqualC.key, Seq(target))

  def label(name: String, stackFrame: Node) = new Node(LabelKey,
    LabelNameKey -> name,
    LabelStackFrame -> stackFrame)

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    LabelC.inject(state)
  }

  def transform(program: Node, state: CompilationState): Unit = {

    val jumpRegistry = CodeAttribute.getState(state).jumpBehaviorRegistry
    def instructionSize(instruction: Node) = CodeAttribute.getInstructionSizeRegistry(state)(instruction.clazz)

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
    val constantPool = clazz.constantPool
    val codeAnnotations: Seq[Node] = CodeAttribute.getCodeAnnotations(clazz)

    for (codeAnnotation <- codeAnnotations) {
      processCodeAnnotation(codeAnnotation)
    }

    def processCodeAnnotation(codeAnnotation: Node): Option[Any] = {
      val instructions = CodeAttribute.getCodeInstructions(codeAnnotation)
      val targetLocations: Map[String, Int] = determineTargetLocations(instructions)
      codeAnnotation(CodeAttribute.CodeAttributesKey) = CodeAttribute.getCodeAttributes(codeAnnotation) ++
        getStackMapTable(constantPool, targetLocations, instructions)

      val newInstructions: Seq[Node] = getNewInstructions(instructions, targetLocations)
      codeAnnotation(CodeAttribute.CodeInstructionsKey) = newInstructions
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

  def getStackMapTable(constantPool: ConstantPool, labelLocations: Map[String, Int], instructions: Seq[Node]): Seq[Node] = {
    val framesPerLocation = instructions.filter(i => i.clazz == LabelKey).
      map(i => (labelLocations(getLabelName(i)), getLabelStackFrame(i))).toMap
    var locationAfterPreviousFrame = 0
    var stackFrames = ArrayBuffer[Node]()
    stackFrames.sizeHint(framesPerLocation.size)
    for (location <- framesPerLocation.keys.toSeq.sorted) {
      val frame = framesPerLocation(location)
      val offset = location - locationAfterPreviousFrame

      frame(StackMapTableAttribute.OffsetDelta) = offset
      stackFrames += frame
      locationAfterPreviousFrame = location + 1
    }
    if (stackFrames.nonEmpty) {
      val nameIndex = constantPool.store(StackMapTableAttribute.stackMapTableId)
      Seq(StackMapTableAttribute.stackMapTable(nameIndex, stackFrames))
    }
    else
      Seq.empty[Node]
  }

  def getLabelStackFrame(label: Node) = label(LabelStackFrame).asInstanceOf[Node]

  def getLabelName(label: Node) = label(LabelNameKey).asInstanceOf[String]

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton, IfIntegerCompareGreaterOrEqualC, GotoC, IfZeroC)

  object LabelC extends InstructionC {
    override val key: Key = LabelKey

    override def getInstructionByteCode(instruction: Node): Seq[Byte] = throw new UnsupportedOperationException()

    override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
      new InstructionSignature(Seq.empty, Seq.empty)
    }

    override def getInstructionSize: Int = 0

    override def getGrammarForThisInstruction(grammars: GrammarCatalogue): BiGrammar = {
      val stackMapTableGrammar = grammars.find(StackMapFrameGrammar)
      nodeMap(name ~> ("(" ~> StringLiteral <~ ")") % stackMapTableGrammar.indent(),
        LabelKey, LabelNameKey, LabelStackFrame)
    }

    override def description: String = "Used to mark a specific point in an instruction list."
  }

  object LabelKey extends Key

  object LabelNameKey extends Key

  object LabelStackFrame extends Key

  override def description: String = "Replaces the jump instructions from bytecode. " +
    "The new instructions are similar to the old ones except that they use labels as target instead of instruction indices."


  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    overrideJumpGrammars(grammars)
    overrideStackMapFrameGrammars(grammars)
  }

  def overrideStackMapFrameGrammars(grammars: GrammarCatalogue): Unit = {
    val delta = grammars.find(DeltaGrammar)
    delta.inner = nodeMap(produce(MissingValue), PartialSelf)
  }

  def overrideJumpGrammars(grammars: GrammarCatalogue) = {
    val jumps = Seq[InstructionC](IfZeroC, IfNotZero, GotoC,
      IfIntegerCompareGreaterOrEqualC,
      IfIntegerCompareLessC, IfIntegerCompareEqualC, IfIntegerCompareNotEqualC)
    for(jump <- jumps)
    {
      val grammar = grammars.find(KeyGrammar(jump.key))
      grammar.inner = jump.name ~> new Consume(StringLiteral).manySeparated(",").inParenthesis ^^ parseMap(jump.key, InstructionArgumentsKey)
    }
  }
}
