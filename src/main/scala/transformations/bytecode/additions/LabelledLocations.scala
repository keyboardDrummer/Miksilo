package transformations.bytecode.additions

import core.bigrammar.{BiGrammar, FromGrammarWithToString}
import core.grammar.StringLiteral
import core.particles._
import core.particles.grammars.{GrammarCatalogue, KeyGrammar}
import core.particles.node.{Key, Node, NodeClass, NodeField}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.attributes.CodeAttribute._
import transformations.bytecode.attributes.StackMapTableAttribute.{StackMapFrameGrammar, offsetGrammarKey}
import transformations.bytecode.attributes.{CodeAttribute, InstructionArgumentsKey, StackMapTableAttribute}
import transformations.bytecode.coreInstructions.integers.integerCompare.IfNotZero.IfNotZeroKey
import transformations.bytecode.coreInstructions.integers.integerCompare._
import transformations.bytecode.coreInstructions.{GotoDelta, InstructionDelta, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object LabelledLocations extends DeltaWithPhase with DeltaWithGrammar with WithState {
  def ifZero(target: String) = instruction(IfZeroDelta.IfZeroKey, Seq(target))
  def ifNotZero(target: String) = instruction(IfNotZeroKey, Seq(target))

  def goTo(target: String) = instruction(GotoDelta.GoToKey, Seq(target))

  def ifIntegerCompareGreaterEquals(target: String) = instruction(IfIntegerCompareGreaterOrEqualDelta.IfIntegerCompareGreaterEqualKey, Seq(target))
  def ifIntegerCompareLess(target: String) = instruction(IfIntegerCompareLessDelta.key, Seq(target))
  def ifIntegerCompareGreater(target: String) = instruction(IfIntegerCompareGreaterDelta$.key, Seq(target))
  def ifIntegerCompareEquals(target: String) = instruction(IfIntegerCompareEqualDelta.key, Seq(target))
  def ifIntegerCompareNotEquals(target: String) = instruction(IfIntegerCompareNotEqualDelta.key, Seq(target))
  def ifIntegerCompareLessEquals(target: String) = instruction(IfIntegerCompareLessOrEqualDelta.key, Seq(target))

  def label(name: String, stackFrame: Node) = new Node(LabelKey,
    LabelName -> name,
    LabelStackFrame -> stackFrame)


  override def createState = mutable.Map.empty
  type State = mutable.Map[Node, mutable.Set[String]]

  def getUniqueLabel(suggestion: String, methodInfo: Node, state: Language): String = {
    val methodCounters = getState(state)
    val taken: mutable.Set[String] = methodCounters.getOrElseUpdate(methodInfo, mutable.Set.empty)
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
    val constantPool = clazz.constantPool
    val codeAnnotations = CodeAttribute.getCodeAnnotations(clazz)

    for (codeAnnotation <- codeAnnotations) {
      processCodeAnnotation(codeAnnotation)
    }

    def processCodeAnnotation(codeAnnotation: Node): Unit = {
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

      frame(StackMapTableAttribute.FrameOffset) = offset
      stackFrames += frame
      locationAfterPreviousFrame = location + 1
    }
    if (stackFrames.nonEmpty) {
      val nameIndex = constantPool.store(StackMapTableAttribute.entry)
      Seq(StackMapTableAttribute.stackMapTable(nameIndex, stackFrames))
    }
    else
      Seq.empty[Node]
  }

  def getLabelStackFrame(label: Node) = label(LabelStackFrame).asInstanceOf[Node]

  def getLabelName(label: Node) = label(LabelName).asInstanceOf[String]

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton, IfIntegerCompareGreaterOrEqualDelta, GotoDelta, IfZeroDelta)

  object LabelDelta extends InstructionDelta {
    override val key: Key = LabelKey

    override def getInstructionByteCode(instruction: Node): Seq[Byte] = throw new UnsupportedOperationException()

    override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature = {
      InstructionSignature(Seq.empty, Seq.empty)
    }

    override def getInstructionSize: Int = 0

    override def getGrammarForThisInstruction(grammars: GrammarCatalogue): BiGrammar = {
      val stackMapFrameGrammar = grammars.find(StackMapFrameGrammar)
      grammarName ~~> (StringLiteral : BiGrammar).as(LabelName) %
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
    val offsetGrammarPaths = grammars.findPathsToKey(offsetGrammarKey)
    offsetGrammarPaths.foreach(delta => delta.removeMeFromSequence())
  }

  def overrideJumpGrammars(grammars: GrammarCatalogue) = {
    val jumps = Seq[InstructionDelta](IfZeroDelta, IfNotZero, GotoDelta,
      IfIntegerCompareGreaterOrEqualDelta,
      IfIntegerCompareLessDelta, IfIntegerCompareEqualDelta, IfIntegerCompareNotEqualDelta)
    for(jump <- jumps)
    {
      val grammar = grammars.find(KeyGrammar(jump.key))
      grammar.inner = jump.grammarName ~~> FromGrammarWithToString(StringLiteral).manySeparated(" ").as(InstructionArgumentsKey) asNode jump.key
    }
  }
}
