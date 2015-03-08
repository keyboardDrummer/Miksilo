package transformations.bytecode.coreInstructions

import core.grammarDocument.BiGrammar
import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import transformations.bytecode.ByteCodeSkeleton.JumpBehavior
import transformations.bytecode._
import transformations.bytecode.attributes.{CodeAttribute, InstructionArgumentsKey}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool
import transformations.types.{ObjectTypeC, TypeSkeleton}

case class InstructionSignature(inputs: Seq[MetaObject], outputs: Seq[MetaObject])

class ByteCodeTypeException(message: String) extends Exception(message)

trait InstructionC extends ParticleWithGrammar {

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getInstructionSignatureRegistry(state).put(key, (constantPool,instruction, stackTypes) => 
      getInstructionInAndOutputs(constantPool, instruction, stackTypes, state))
    ByteCodeSkeleton.getState(state).getBytes.put(key, getInstructionByteCode)
    ByteCodeSkeleton.getInstructionSizeRegistry(state).put(key, getInstructionSize)
    ByteCodeSkeleton.getState(state).jumpBehaviorRegistry.put(key, getJumpBehavior)
    ByteCodeSkeleton.getState(state).localUpdates.put(key, (instruction: MetaObject, stackTypes) => getVariableUpdates(instruction, stackTypes))
  }

  def assertObjectTypeStackTop(stackTop: MetaObject, name: String): Unit = {
    if (stackTop.clazz != ObjectTypeC.ObjectTypeKey)
      throw new ByteCodeTypeException(s"$name requires an object on top of the stack and not a $stackTop.")
  }

  def assertDoubleWord(state: CompilationState, input: MetaObject): Unit = {
    if (TypeSkeleton.getTypeSize(input, state) != 2) {
      throw new ByteCodeTypeException("expected double word input")
    }
  }

  def assertSingleWord(state: CompilationState, input: MetaObject): Unit = {
    if (TypeSkeleton.getTypeSize(input, state) != 1) {
      throw new ByteCodeTypeException("expected single word input")
    }
  }

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  val key: AnyRef

  def getVariableUpdates(instruction: MetaObject, typeState: ProgramTypeState): Map[Int, MetaObject] = Map.empty

  def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature

  def getInstructionSize(instruction: MetaObject): Int = getInstructionByteCode(instruction).size

  def getJumpBehavior: JumpBehavior = new JumpBehavior(true, false)

  def getInstructionByteCode(instruction: MetaObject): Seq[Byte]

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val instructionGrammar = grammars.find(CodeAttribute.InstructionGrammar)
    instructionGrammar.addOption(getGrammarForThisInstruction)
  }

  def getGrammarForThisInstruction: BiGrammar = {
    name ~> integer.manySeparated(",").inParenthesis ^^ parseMap(key, InstructionArgumentsKey)
  }

  protected def binary(_type: MetaObject) = InstructionSignature(Seq(_type, _type), Seq(_type))

  override def description: String = s"Defines the $name instruction."
}
